-- | Generating and parsing Value Change Dump (VCD) files.
module Data.VCD
  ( 
  -- * VCD Generation
    VCDHandle
  , Timescale (..)
  , newVCD
  , scope
  , var
  , step
  -- * VCD Parsing
  , VCD        (..)
  , Definition (..)
  , Value      (..)
  , parseVCD
  ) where

import Control.Monad
import Data.Bits
import Data.Char
import Data.Int
import Data.IORef
import Data.Word
import System.IO
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Pos
import Text.Printf

data VCDHandle = VCDHandle
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

assertDefs :: VCDHandle -> IO ()
assertDefs vcd = do
  defs <- readIORef $ defs vcd
  when (not defs) $ error "VCD variable definition in recording phase"

assertNotDefs :: VCDHandle -> IO ()
assertNotDefs vcd = do
  defs <- readIORef $ defs vcd
  when defs $ error "VCD variable recording in definition phase"

nextCode :: VCDHandle -> IO String
nextCode vcd = do
  assertDefs vcd
  codes' <- readIORef $ codes vcd
  writeIORef (codes vcd) (tail codes')
  return $ head codes'

class    Signal a      where var :: VCDHandle -> String -> a -> IO (a -> IO ())
instance Signal Bool   where var = package "wire" 1 (\ a -> if a then "1" else "0")
instance Signal Int    where var vcd name init = package "integer" (bitSize init) bitString vcd name init
instance Signal Int8   where var = package "integer"  8 bitString
instance Signal Int16  where var = package "integer" 16 bitString
instance Signal Int32  where var = package "integer" 16 bitString
instance Signal Int64  where var = package "integer" 16 bitString
instance Signal Word8  where var = package "wire"     8 bitString
instance Signal Word16 where var = package "wire"    16 bitString
instance Signal Word32 where var = package "wire"    32 bitString
instance Signal Word64 where var = package "wire"    64 bitString
instance Signal Float  where var = package "real" 32 (\ a -> "r" ++ show a ++ " ")
instance Signal Double where var = package "real" 64 (\ a -> "r" ++ show a ++ " ")

bitString :: Bits a => a -> String
bitString n = "b" ++ (if null bits then "0" else bits) ++ " "
  where
  bits = dropWhile (== '0') $ [ if testBit n i then '1' else '0' | i <- [bitSize n - 1, bitSize n - 2 .. 0] ]

package :: Eq a => String -> Int -> (a -> String) -> VCDHandle -> String -> a -> IO (a -> IO ())
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


newVCD :: Handle -> Timescale -> IO VCDHandle
newVCD h ts = do
  hPutStrLn h $ "$timescale"
  hPutStrLn h $ "  1 " ++ show ts
  hPutStrLn h $ "$end"
  defs     <- newIORef True
  dirty    <- newIORef True
  time     <- newIORef 0
  codes    <- newIORef identCodes
  dumpvars <- newIORef $ return ()
  return VCDHandle
    { handle   = h
    , defs     = defs
    , dirty    = dirty
    , time     = time
    , codes    = codes
    , dumpvars = dumpvars
    }

scope :: VCDHandle -> String -> IO a -> IO a
scope vcd name a = do
  hPutStrLn (handle vcd) $ "$scope module " ++ name ++ " $end"
  a <- a
  hPutStrLn (handle vcd) $ "$upscope $end"
  return a

step :: VCDHandle -> Int -> IO ()
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

data Token
  = End
  | Timescale
  | Scope'
  | Var
  | UpScope
  | EndDefinitions
  | DumpVars
  | Step Int
  | String String
  deriving (Show, Eq)

type VCDParser = GenParser Token ()

-- | Parse VCD data.
parseVCD :: String -> VCD
parseVCD a = case parse vcd "unknown" $ map token $ words a of
  Left err -> error $ show err
  Right vcd -> vcd
  where
  token a = case a of
    "$end"                -> End
    "$timescale"          -> Timescale
    "$scope"              -> Scope'
    "$var"                -> Var
    "$upscope"            -> UpScope
    "$enddefinitions"     -> EndDefinitions
    "$dumpvars"           -> DumpVars
    '#':a | not (null a) && all isDigit a -> Step $ read a
    a                     -> String a

noPos :: a -> SourcePos
noPos _ = initialPos "unknown"

tok' = token show noPos
tok a = tok' (\ b -> if a == b then Just () else Nothing)
str = tok' $ \ a -> case a of
  String a -> Just a
  _        -> Nothing

data VCD = VCD Timescale [Definition] [(Int, [(String, Value)])] deriving Show

data Value = Bool Bool | Bits [Bool] | Double Double deriving Show

--data Definition = Scope String [Definition] | Var String
data Definition = Definition deriving Show

vcd :: VCDParser VCD
vcd = do
  ts <- timescale
  defs <- definitions
  tok EndDefinitions
  tok End
  tok DumpVars
  initValues <- values
  tok End
  initTime <- step'
  samples <- many sample >>= return . ((initTime, initValues):)
  eof
  return $ VCD ts defs samples

timescale :: VCDParser Timescale
timescale = do
  tok Timescale
  one <- str
  sc  <- str
  tok End
  when (one /= "1") $ error $ "invalid timescale: " ++ one
  case sc of
    "s"  -> return S
    "ms" -> return MS
    "us" -> return US
    "ps" -> return PS
    _    -> error $ "invalid timescale: " ++ sc

definitions :: VCDParser [Definition]
definitions = many $ scope' <|> var'

scope' :: VCDParser Definition
scope' = do
  tok Scope'
  str
  name <- str
  tok End
  defs <- definitions
  tok UpScope
  tok End
  return Definition

var' :: VCDParser Definition
var' = do
  tok Var
  typ   <- str
  width <- str
  code  <- str
  name  <- str
  tok End
  return Definition

step' :: VCDParser Int
step' = tok' $ \ a -> case a of
  Step a -> Just a
  _      -> Nothing

sample :: VCDParser (Int, [(String, Value)])
sample = do
  a <- values
  i <- step'
  return (i, a)

values :: VCDParser [(String, Value)]
values = many str >>= return . values'

values' :: [String] -> [(String, Value)]
values' a = case a of
  [] -> []
  ('0':code):a -> (code, Bool False) : values' a
  ('1':code):a -> (code, Bool True ) : values' a
  ('b':bits):code:a  -> (code, Bits [ b == '1' | b <- bits ]) : values' a
  ('r':float):code:a -> (code, Double  $ read float)  : values' a
  (a:_) -> error $ "invalid value: " ++ a

