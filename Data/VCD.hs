module Data.VCD
  ( VCD
  , Timescale (..)
  , newVCD
  , level
  , signal
  , step
  ) where

import Control.Monad
import Data.Bits
import Data.Char
import Data.Int
import Data.IORef
import Data.Word
import System.IO
import Text.Printf

data VCD = VCD
  { handle :: Handle
  , defs   :: IORef Bool
  , dirty  :: IORef Bool
  , time   :: IORef Int
  , codes  :: IORef [String]
  }

data Timescale = US | MS | S

instance Show Timescale where
  show US = "us"
  show MS = "ms"
  show S  = "s"

assertDefs :: VCD -> IO ()
assertDefs vcd = do
  defs <- readIORef $ defs vcd
  when (not defs) $ error "VCD signal definition in recording phase"

assertNotDefs :: VCD -> IO ()
assertNotDefs vcd = do
  defs <- readIORef $ defs vcd
  when defs $ error "VCD signal recording in definition phase"

nextCode :: VCD -> IO String
nextCode vcd = do
  assertDefs vcd
  codes' <- readIORef $ codes vcd
  writeIORef (codes vcd) (tail codes')
  return $ head codes'

class Signal a where signal :: VCD -> String -> IO (a -> IO ())

instance Signal Bool where
  signal vcd name = do
    code <- nextCode vcd
    hPutStrLn (handle vcd) $ printf "$var wire 1 %s %s $end" code name
    last <- newIORef Nothing
    return $ \ a -> do
      assertNotDefs vcd
      last' <- readIORef last
      when (last' /= Just a) $ do
        hPutStrLn (handle vcd) $ (if a then "1" else "0") ++ code
        writeIORef last $ Just a
        writeIORef (dirty vcd) True

bitsSignal :: Bits a => VCD -> String -> IO (a -> IO ())
bitsSignal vcd name = do
  let dumb = 0
  code <- nextCode vcd
  hPutStrLn (handle vcd) $ printf "$var %s %d %s %s $end" (if isSigned dumb then "integer" else "wire") (bitSize dumb) code name
  last <- newIORef Nothing
  return $ \ a -> do
    assertNotDefs vcd
    when (a == dumb) (return ())
    last' <- readIORef last
    when (last' /= Just a) $ do
      hPutStrLn (handle vcd) $ printf "b%s %s" (bitString a) code
      writeIORef last $ Just a
      writeIORef (dirty vcd) True

bitString :: Bits a => a -> String
bitString n = if null bits then "0" else bits
  where
  bits = dropWhile (== '0') $ [ if testBit n i then '1' else '0' | i <- [bitSize n - 1, bitSize n - 2 .. 0] ]

instance Signal Int    where signal = bitsSignal
instance Signal Int8   where signal = bitsSignal
instance Signal Int16  where signal = bitsSignal
instance Signal Int32  where signal = bitsSignal
instance Signal Int64  where signal = bitsSignal
instance Signal Word8  where signal = bitsSignal
instance Signal Word16 where signal = bitsSignal
instance Signal Word32 where signal = bitsSignal
instance Signal Word64 where signal = bitsSignal
   
instance Signal Float where
  signal vcd name = do
    code <- nextCode vcd
    hPutStrLn (handle vcd) $ printf "$var real 32 %s %s $end" code name
    last <- newIORef Nothing
    return $ \ a -> do
      assertNotDefs vcd
      last' <- readIORef last
      when (last' /= Just a) $ do
        hPutStrLn (handle vcd) $ printf "r%s %s" (show a) code
        writeIORef last $ Just a
        writeIORef (dirty vcd) True

instance Signal Double where
  signal vcd name = do
    code <- nextCode vcd
    hPutStrLn (handle vcd) $ printf "$var real 64 %s %s $end" code name
    last <- newIORef Nothing
    return $ \ a -> do
      assertNotDefs vcd
      last' <- readIORef last
      when (last' /= Just a) $ do
        hPutStrLn (handle vcd) $ printf "r%s %s" (show a) code
        writeIORef last $ Just a
        writeIORef (dirty vcd) True


newVCD :: Handle -> Timescale -> IO VCD
newVCD h ts = do
  hPutStrLn h $ "$timescale"
  hPutStrLn h $ "  1 " ++ show ts
  hPutStrLn h $ "$end"
  defs  <- newIORef True
  dirty <- newIORef True
  time  <- newIORef 0
  codes <- newIORef identCodes
  return VCD
    { handle = h
    , defs   = defs
    , dirty  = dirty
    , time   = time
    , codes  = codes
    }

level :: VCD -> String -> IO a -> IO a
level vcd name a = do
  hPutStrLn (handle vcd) $ "$scope module " ++ name ++ " $end"
  a <- a
  hPutStrLn (handle vcd) $ "$upscope $end"
  return a

step :: VCD -> Int -> IO ()
step vcd n = do
  defs' <- readIORef $ defs vcd
  when defs' $ do
    hPutStrLn (handle vcd) "$enddefinitions $end"
    writeIORef (defs vcd) False

  dirty' <- readIORef $ dirty vcd
  when dirty' $ do
    t <- readIORef $ time vcd
    hPutStrLn (handle vcd) $ "#" ++ show (t + n)
    writeIORef (dirty vcd) False
    writeIORef (time vcd) $ t + n
    hFlush $ handle vcd

identCodes :: [String]
identCodes = map code [0..]
  where
  code :: Int -> String
  code i | i < 94 =           [chr (33 + mod i 94)] 
  code i = code (div i 94) ++ [chr (33 + mod i 94)] 


{-
writeVCD :: Handle -> Handle -> String -> String -> [String] -> [Type] -> (String -> [String]) -> (String -> Int) -> IO ()
writeVCD i o date version names' types' f toTime = do
  hPutStrLn o $ "$date"
  hPutStrLn o date
  hPutStrLn o $ "$end"
  hPutStrLn o $ "$version"
  hPutStrLn o version
  hPutStrLn o $ "$end"
  hPutStrLn o $ "$timescale"
  hPutStrLn o $ "  1 ms"
  hPutStrLn o $ "$end"
  mapM_ writeVarDecl $ zip3 names types identCodes
  hPutStrLn o $ "$enddefinitions $end"
  hFlush o
  writeSamples 0 (replicate (length names) "")
  hClose o
  where
  names = tail names'
  types = tail types'

  writeVarDecl :: (String, Type, String) -> IO ()
  writeVarDecl (name, t, code) = hPutStrLn o $ case t of
    Bool   -> "$var wire 1 "     ++ code ++ " " ++ name ++ " $end"
    Int8   -> "$var integer 8 "  ++ code ++ " " ++ name ++ " $end"
    Int16  -> "$var integer 16 " ++ code ++ " " ++ name ++ " $end"
    Int32  -> "$var integer 32 " ++ code ++ " " ++ name ++ " $end"
    Int64  -> "$var integer 64 " ++ code ++ " " ++ name ++ " $end"
    Word8  -> "$var wire 8 "     ++ code ++ " " ++ name ++ " $end"
    Word16 -> "$var wire 16 "    ++ code ++ " " ++ name ++ " $end"
    Word32 -> "$var wire 32 "    ++ code ++ " " ++ name ++ " $end"
    Word64 -> "$var wire 64 "    ++ code ++ " " ++ name ++ " $end"
    Float  -> "$var real 32 "    ++ code ++ " " ++ name ++ " $end"
    Double -> "$var real 64 "    ++ code ++ " " ++ name ++ " $end"

  writeSamples :: Int -> [String] -> IO ()
  writeSamples lastTime lastValues = do
    eof <- hIsEOF i
    case eof of
      True  -> hPutStrLn o $ "#" ++ show (lastTime + 1)
      False -> do
        l <- hGetRealLine i
        let values' = f l
            time = (toTime $ head values') :: Int
            values = tail values'
        when (values /= lastValues) $ hPutStrLn o $ "#" ++ show time
        mapM_ writeValue $ zip4 types identCodes values lastValues  --XXX Would not work if first value is invalid.
        hFlush o
        writeSamples time values

  writeValue :: (Type, String, String, String) -> IO ()
  writeValue (t, c, v, vl) | v == vl                = return ()
                           | otherwise              = case t of
    Bool    -> hPutStrLn o $ v ++ c
    Float   -> hPutStrLn o $ "r" ++ v ++ " " ++ c
    Double  -> hPutStrLn o $ "r" ++ v ++ " " ++ c
    _       -> hPutStrLn o $ "b" ++ bitString (read v) ++ " " ++ c
  
-}
