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
  { handle   :: Handle
  , defs     :: IORef Bool
  , dirty    :: IORef Bool
  , time     :: IORef Int
  , codes    :: IORef [String]
  , dumpvars :: IORef (IO ())
  }

data Timescale = S | MS | US | PS

instance Show Timescale where
  show S  = "s"
  show MS = "ms"
  show US = "us"
  show PS = "ps"

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

class    Signal a      where signal :: VCD -> String -> a -> IO (a -> IO ())
instance Signal Bool   where signal = package "wire" 1 (\ a -> if a then "1" else "0")
instance Signal Int    where signal vcd name init = package "integer" (bitSize init) bitString vcd name init
instance Signal Int8   where signal = package "integer"  8 bitString
instance Signal Int16  where signal = package "integer" 16 bitString
instance Signal Int32  where signal = package "integer" 16 bitString
instance Signal Int64  where signal = package "integer" 16 bitString
instance Signal Word8  where signal = package "wire"     8 bitString
instance Signal Word16 where signal = package "wire"    16 bitString
instance Signal Word32 where signal = package "wire"    32 bitString
instance Signal Word64 where signal = package "wire"    64 bitString
instance Signal Float  where signal = package "real" 32 (\ a -> "r" ++ show a ++ " ")
instance Signal Double where signal = package "real" 64 (\ a -> "r" ++ show a ++ " ")

bitString :: Bits a => a -> String
bitString n = "b" ++ (if null bits then "0" else bits) ++ " "
  where
  bits = dropWhile (== '0') $ [ if testBit n i then '1' else '0' | i <- [bitSize n - 1, bitSize n - 2 .. 0] ]

package :: Eq a => String -> Int -> (a -> String) -> VCD -> String -> a -> IO (a -> IO ())
package typ width value vcd name init = do
  code <- nextCode vcd
  hPutStrLn (handle vcd) $ printf "$var %s %d %s %s $end" typ width code name
  last <- newIORef Nothing
  let sample a = do assertNotDefs vcd
                    last' <- readIORef last
                    when (last' /= Just a) $ do
                      hPutStrLn (handle vcd) $ value a ++ code
                      writeIORef last $ Just a
                      writeIORef (dirty vcd) True
  modifyIORef (dumpvars vcd) (\ a -> a >> sample init)
  return sample


newVCD :: Handle -> Timescale -> IO VCD
newVCD h ts = do
  hPutStrLn h $ "$timescale"
  hPutStrLn h $ "  1 " ++ show ts
  hPutStrLn h $ "$end"
  defs     <- newIORef True
  dirty    <- newIORef True
  time     <- newIORef 0
  codes    <- newIORef identCodes
  dumpvars <- newIORef $ return ()
  return VCD
    { handle   = h
    , defs     = defs
    , dirty    = dirty
    , time     = time
    , codes    = codes
    , dumpvars = dumpvars
    }

level :: VCD -> String -> IO a -> IO a
level vcd name a = do
  hPutStrLn (handle vcd) $ "$scope module " ++ name ++ " $end"
  a <- a
  hPutStrLn (handle vcd) $ "$upscope $end"
  return a

step :: VCD -> Int -> IO ()
step vcd n = do
  defs'     <- readIORef $ defs     vcd
  dumpvars' <- readIORef $ dumpvars vcd
  when defs' $ do
    writeIORef (defs vcd) False
    hPutStrLn (handle vcd) "$enddefinitions $end"
    hPutStrLn (handle vcd) "$dumpvars"
    dumpvars'
    hPutStrLn (handle vcd) "$end"

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

