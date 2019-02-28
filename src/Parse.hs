module Parse where

import           Data.Char                    (isDigit)
import           Text.ParserCombinators.ReadP

getString = "This is a test!"

data PressureType
  = CpuPressure
  | IOPressure
  | MemoryPressure
  deriving (Show, Eq)

data SomeOrFull
  = Some
  | Full
  deriving (Show, Eq)

-- CPU --
data Pressure = Pressure
  { pressureAvg10       :: Double
  , pressureAvg60       :: Double
  , pressureAvg300      :: Double
  , pressureAbsUSeconds :: Int -- Absolute pressure in microseconds
  , pressurePType       :: PressureType
  , pressureSomeFull    :: SomeOrFull
  } deriving (Show)

data LoadAvg = LoadAvg
  { loadAvg60           :: Double
  , loadAvg300          :: Double
  , loadAvg900          :: Double
  , loadAvgRunThreads   :: Int
  , loadAvgTotalThreads :: Int
  } deriving (Show)

sampleLoadAvg = "1.03 1.40 0.76 3/956 24613"

readLoad :: ReadP LoadAvg
readLoad = do
  a60 <- double
  satisfy (== ' ')
  a300 <- double
  satisfy (== ' ')
  a900 <- double
  satisfy (== ' ')
  running <- int
  satisfy (== '/')
  total <- int
  return (LoadAvg a60 a300 a900 running total)

parseLoad :: String -> LoadAvg
parseLoad loadTxt = fst $ head p
  where
    p = readP_to_S readLoad loadTxt

sampleCpu = "some avg10=0.02 avg60=0.05 avg300=0.01 total=125610913"

readCpu :: ReadP [Pressure]
readCpu = do
  string "some "
  a10v <- parseKeyDouble "avg10"
  a60v <- parseKeyDouble "avg60"
  a300v <- parseKeyDouble "avg300"
  abs_ms_v <- parseKeyInt "total"
  return [Pressure a10v a60v a300v abs_ms_v CpuPressure Some]

parseCpu :: String -> [Pressure]
parseCpu cpuText = fst $ head p
  where
    p = readP_to_S readCpu cpuText

-- IO / Memory--
-- Sample Data --
parseMeIO =
  "some avg10=0.00 avg60=0.00 avg300=0.00 total=50730573\nfull avg10=0.00 avg60=0.00 avg300=0.00 total=47476717"

parseMeMemory =
  "some avg10=0.00 avg60=0.00 avg300=0.00 total=0\nfull avg10=0.00 avg60=0.00 avg300=0.00 total=0"

readIOorMemory :: PressureType -> ReadP [Pressure]
readIOorMemory ptype = do
  string "some "
  a10vs <- parseKeyDouble "avg10"
  a60vs <- parseKeyDouble "avg60"
  a300vs <- parseKeyDouble "avg300"
  abs_ms_vs <- parseKeyInt "total"
  string "\nfull "
  a10vf <- parseKeyDouble "avg10"
  a60vf <- parseKeyDouble "avg60"
  a300vf <- parseKeyDouble "avg300"
  abs_ms_vf <- parseKeyInt "total"
  return
    [ (Pressure a10vf a60vf a300vf abs_ms_vf ptype Some)
    , (Pressure a10vf a60vf a300vf abs_ms_vf ptype Full)
    ]

parseIOorMemory :: PressureType -> String -> [Pressure]
parseIOorMemory ptype text = fst $ head p
  where
    readThing = readIOorMemory ptype
    p = readP_to_S readThing text

parseIO :: String -> [Pressure]
parseIO = parseIOorMemory IOPressure

parseMemory :: String -> [Pressure]
parseMemory = parseIOorMemory MemoryPressure

-- Parses like "temp=98.78 "
-- Pass in the key name, here it is "temp".
-- Reads the double and assumes there's always a space after
parseKeyDouble :: String -> ReadP Double
parseKeyDouble key = do
  string $ key ++ "="
  (s, _) <- gather parse_double
  satisfy (== ' ')
  return $ (read s :: Double)

-- Parses like "height=1234"
-- Pass in the key name, here it is "height".
-- Reads the int.
parseKeyInt :: String -> ReadP Int
parseKeyInt key = do
  string $ key ++ "="
  s <- munch1 isDigit
  return $ (read s :: Int)

parse_double :: ReadP ()
parse_double = do
  option '+' (char '+' +++ char '-')
  munch isDigit
  optional (char '.' >> munch1 isDigit)
  optional (char 'e' >> option '+' (char '+' +++ char '-') >> int)

double :: ReadP Double
double = do
  (s, _) <- gather parse_double
  return $ (read s :: Double)

int :: ReadP Int
int = do
  s <- munch1 isDigit
  return $ (read s :: Int)
