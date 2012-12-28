-- | Generating and parsing Value Change Dump (VCD) files.
module Data.VCD
  ( 
  -- * VCD Generation
    VCDHandle
  , Timescale (..)
  , Variable (..)
  , variable
  , newVCD
  , step
  , step'
  -- * VCD Parsing
  , VCD        (..)
  , Definition (..)
  , Value      (..)
  , parseVCD
  ) where

import Control.Monad
import Data.Bits
import Data.Char
import Data.IORef
import System.IO
import Text.ParserCombinators.Poly.Lazy
import Text.Printf

import Data.VCD.Tree

-- | The VCDHandle keeps track of generation state and the output handle.
data VCDHandle = VCDHandle
  { handle   :: Handle
  , defs     :: IORef Bool
  , dirty    :: IORef Bool
  , time     :: IORef Int
  , codes    :: IORef [String]
  , dumpvars :: IORef (IO ())
  , vars     :: IORef [(String, Int, String, [String])] -- ^ (typ, width, code, path)
  }

-- | VCD Timescale.
data Timescale
  = S     -- ^ seconds
  | MS    -- ^ milliseconds
  | US    -- ^ microseconds
  | PS    -- ^ picoseconds

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

-- | Types that can be recorded as VCD variables.
class Variable a where
  -- | Define a new variable.
  var :: VCDHandle -> [String] -> a -> IO (a -> IO ())

instance Bits a => Variable a where var vcd path init = variable (if isSigned init then "integer" else "wire") (bitSize init) bitString vcd path init
instance Variable Bool   where var = variable "wire"  1 (\ a -> if a then "1" else "0")
instance Variable Float  where var = variable "real" 32 (\ a -> "r" ++ show a ++ " ")
instance Variable Double where var = variable "real" 64 (\ a -> "r" ++ show a ++ " ")

bitString :: Bits a => a -> String
bitString n = "b" ++ (if null bits then "0" else bits) ++ " "
  where
  bits = dropWhile (== '0') $ [ if testBit n i then '1' else '0' | i <- [bitSize n - 1, bitSize n - 2 .. 0] ]

-- | Helper to create new 'Variable' instances.
variable :: Eq a => String -> Int -> (a -> String) -> VCDHandle -> [String] -> a -> IO (a -> IO ())
variable typ width value vcd path init = do
  code <- nextCode vcd
  modifyIORef (vars vcd) ((typ, width, code, path) :)
  last <- newIORef Nothing
  let sample a = do assertNotDefs vcd
                    last' <- readIORef last
                    when (last' /= Just a) $ do
                      hPutStrLn (handle vcd) $ value a ++ code
                      writeIORef last $ Just a
                      writeIORef (dirty vcd) True
  modifyIORef (dumpvars vcd) (\ a -> a >> sample init)
  return sample

-- | Create a new handle for generating a VCD file with a given timescale.
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
  vars     <- newIORef []
  return VCDHandle
    { handle   = h
    , defs     = defs
    , dirty    = dirty
    , time     = time
    , codes    = codes
    , dumpvars = dumpvars
    , vars     = vars
    }

stepInit :: VCDHandle -> IO ()
stepInit vcd = do
  defs'     <- readIORef $ defs     vcd
  dumpvars' <- readIORef $ dumpvars vcd
  vars'     <- readIORef $ vars     vcd
  when defs' $ do
    writeIORef (defs vcd) False
    mapM_ (defineVar $ handle vcd) $ tree (\ (_, _, _, a) -> a) vars'
    hPutStrLn (handle vcd) "$enddefinitions $end"
    hPutStrLn (handle vcd) "$dumpvars"
    dumpvars'
    hPutStrLn (handle vcd) "$end"
    writeIORef (dirty vcd) True

defineVar :: Handle -> Tree String (String, Int, String, [String]) -> IO ()
defineVar h a = case a of
  Branch name subs -> do
    hPutStrLn h $ "$scope module " ++ name ++ " $end"
    mapM_ (defineVar h) subs
    hPutStrLn h $ "$upscope $end"
  Leaf name (typ, width, code, _) -> hPutStrLn h $ printf "$var %s %d %s %s $end" typ width code name

-- | Set a time step.  'step' will also transition a VCDHandle from the definition to the recording phase.
step :: VCDHandle -> Int -> IO ()
step vcd n = do
  stepInit vcd
  t <- readIORef $ time vcd
  writeIORef (time vcd) $ t + n
  dirty' <- readIORef $ dirty vcd
  when dirty' $ do
    hPutStrLn (handle vcd) $ "#" ++ show (t + n)
    writeIORef (dirty vcd) False
    hFlush $ handle vcd

-- | Save as 'step', but forces a step recording even if variables have not changed.  Useful for realtime simulation.
step' :: VCDHandle -> Int -> IO ()
step' vcd n = do
  stepInit vcd
  t <- readIORef $ time vcd
  writeIORef (time vcd) $ t + n
  hPutStrLn (handle vcd) $ "#" ++ show (t + n)
  hFlush $ handle vcd

identCodes :: [String]
identCodes = map code [0..]
  where
  code :: Int -> String
  code i | i < 94 =           [chr (33 + mod i 94)] 
  code i = code (div i 94) ++ [chr (33 + mod i 94)] 

-- | VCD database.
data VCD = VCD Timescale [Definition] [(Int, [(String, Value)])] deriving Show

-- | Recorded value.
data Value = Bool Bool | Bits [Bool] | Double Double deriving Show

-- | Variable definition.
data Definition
  = Scope String [Definition]     -- ^ Hierarchical scope.
  | Var String Int String String  -- ^ Variable with type, width, code, name.
  deriving Show

data Token
  = End
  | Timescale
  | Scope'
  | Var'
  | UpScope
  | EndDefinitions
  | DumpVars
  | Step Int
  | String String
  deriving (Show, Eq)

type VCDParser = Parser Token

-- | Parse VCD data.
parseVCD :: String -> VCD
parseVCD a = fst $ runParser vcd $ map token $ words a
  where
  token a = case a of
    "$end"                -> End
    "$timescale"          -> Timescale
    "$scope"              -> Scope'
    "$var"                -> Var'
    "$upscope"            -> UpScope
    "$enddefinitions"     -> EndDefinitions
    "$dumpvars"           -> DumpVars
    '#':a | not (null a) && all isDigit a -> Step $ read a
    a                     -> String a

tok :: Token -> VCDParser ()
tok a = satisfy (== a) >> return ()

str :: VCDParser String
str = do
  String sc <- satisfy (\ a -> case a of { String _ -> True; _ -> False })
  return sc

vcd :: VCDParser VCD
vcd = return (\ ts defs initValues initTime samples -> VCD ts defs $ (initTime, initValues) : samples)
  `apply`   timescale
  `apply`   definitions
  `discard` tok EndDefinitions
  `discard` tok End
  `discard` tok DumpVars
  `apply`   values
  `discard` tok End
  `apply`   step_
  `apply`   many sample
  `discard` eof

timescale :: VCDParser Timescale
timescale = do
  tok Timescale
  tok $ String "1"
  sc <- str
  tok End
  case sc of
    "s"  -> return S
    "ms" -> return MS
    "us" -> return US
    "ps" -> return PS
    _    -> error $ "invalid timescale: " ++ sc

definitions :: VCDParser [Definition]
definitions = many $ oneOf [scope_, var_]

scope_ :: VCDParser Definition
scope_ = do
  tok Scope'
  str
  name <- str
  tok End
  defs <- definitions
  tok UpScope
  tok End
  return $ Scope name defs

var_ :: VCDParser Definition
var_ = do
  tok Var'
  typ   <- str
  width <- str
  code  <- str
  name  <- str
  tok End
  return $ Var typ (read width) code name

step_ :: VCDParser Int
step_ = do
  Step a <- satisfy (\ a -> case a of { Step _ -> True; _ -> False })
  return a

sample :: VCDParser (Int, [(String, Value)])
sample = do
  a <- values
  i <- step_
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

