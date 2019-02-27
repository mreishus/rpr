module Main where

import           Control.Monad          (forM_)
import           Control.Monad.IO.Class
import           Debug.Trace            (trace)
import           GHC.Int
import           Lib
import           Numeric                (showFFloat)
import           Parse                  (LoadAvg (..), Pressure,
                                         PressureType (..), SomeOrFull (..),
                                         avg10, avg300, avg60, getString,
                                         parseCpu, parseIO, parseLoad,
                                         parseMemory, pressure_of, some_full)
import           System.Directory       (doesFileExist)
import           Text.Printf
import           UI.NCurses

colors =
  [ ColorMagenta
  , ColorRed
  , ColorYellow
  , ColorGreen
  , ColorBlue
  , ColorCyan
  , ColorWhite
  , ColorBlack
  ]

--readFile  :: FilePath -> IO String
getCpuPressure :: IO String
getCpuPressure = readFile "/proc/pressure/cpu"

getPressures :: IO [Pressure]
getPressures = do
  cpuText <- readFile "/proc/pressure/cpu"
  memoryText <- readFile "/proc/pressure/memory"
  ioText <- readFile "/proc/pressure/io"
  return $ concat [parseMemory memoryText, parseIO ioText, parseCpu cpuText]

getLoad :: IO LoadAvg
getLoad = do
  loadText <- readFile "/proc/loadavg"
  return $ parseLoad loadText

main :: IO ()
main = do
  supportsLoad <- doesFileExist "/proc/pressure/cpu"
  if not supportsLoad
    then do
      putStrLn "This system doesn't support linux pressure stall information."
      putStrLn "You'll need linux 4.20 or later."
    else runCurses $ do
           setEcho False
           loop
  where
    p ev = ev == EventCharacter 'q' || ev == EventCharacter 'Q'
    loop = do
      w <- defaultWindow
      pressures <- liftIO getPressures
      myRender w pressures
      render
      ev <- getEvent w (Just 1000)
      case ev of
        Nothing -> loop
        Just ev' ->
          if p ev'
            then return ()
            else loop

renderPressure :: [Pressure] -> PressureType -> SomeOrFull -> String
renderPressure ps pt sorf = a10 ++ " " ++ a60 ++ " " ++ a300
  where
    c = head $ filter (\x -> pressure_of x == pt && some_full x == sorf) ps
    a10 = showDec $ avg10 c
    a60 = showDec $ avg60 c
    a300 = showDec $ avg300 c

renderCpuPressure :: [Pressure] -> String
renderCpuPressure ps = renderPressure ps CpuPressure Some

renderMemoryPressure :: [Pressure] -> SomeOrFull -> String
renderMemoryPressure ps sf = renderPressure ps MemoryPressure sf

renderIOPressure :: [Pressure] -> SomeOrFull -> String
renderIOPressure ps sf = renderPressure ps IOPressure sf

--showDec :: Double -> String
--showDec = printf "%.2f"
showDec :: Double -> String
showDec x = showFFloat (Just 2) x ""

myRender :: Window -> [Pressure] -> Curses ()
myRender window pressures =
  updateWindow window $ do
    moveCursor 0 1
    drawString "pressure-stall info"
    moveCursor 2 10
    drawString "10s   1m   5m"
    moveCursor 3 1
    drawString "cpu:    "
    drawString $ renderCpuPressure pressures ++ " some"
    moveCursor 5 1
    drawString "memory: "
    drawString $ renderMemoryPressure pressures Some ++ " some"
    moveCursor 6 9
    drawString $ renderMemoryPressure pressures Full ++ " full"
    moveCursor 8 1
    drawString "io:     "
    drawString $ renderIOPressure pressures Some ++ " some"
    moveCursor 9 9
    drawString $ renderIOPressure pressures Full ++ " full"
    moveCursor 11 1
    drawString "(press q to quit)"
    moveCursor 0 0
