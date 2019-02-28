module Main where

import           Control.Monad          (forM_)
import           Control.Monad.IO.Class
import           GHC.Int
import           Numeric                (showFFloat)
import           Parse                  (LoadAvg (..), Pressure,
                                         PressureType (..), SomeOrFull (..),
                                         getString, parseCpu, parseIO,
                                         parseLoad, parseMemory, pressureAvg10,
                                         pressureAvg300, pressureAvg60,
                                         pressurePType, pressureSomeFull)
import           System.Directory       (doesFileExist)
import           Text.Printf
import           UI.NCurses

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
      loadA <- liftIO getLoad
      myRender w pressures loadA
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
    c =
      head $
      filter (\x -> pressurePType x == pt && pressureSomeFull x == sorf) ps
    a10 = showDec $ pressureAvg10 c
    a60 = showDec $ pressureAvg60 c
    a300 = showDec $ pressureAvg300 c

renderCpuPressure :: [Pressure] -> String
renderCpuPressure ps = renderPressure ps CpuPressure Some

renderMemoryPressure :: [Pressure] -> SomeOrFull -> String
renderMemoryPressure ps sf = renderPressure ps MemoryPressure sf

renderIOPressure :: [Pressure] -> SomeOrFull -> String
renderIOPressure ps sf = renderPressure ps IOPressure sf

renderLoad :: LoadAvg -> String
renderLoad l = a60 ++ " " ++ a300 ++ " " ++ a900
  where
    a60 = showDec $ loadAvg60 l
    a300 = showDec $ loadAvg300 l
    a900 = showDec $ loadAvg900 l

--showDec :: Double -> String
--showDec = printf "%.2f"
showDec :: Double -> String
showDec x = showFFloat (Just 2) x ""

myRender :: Window -> [Pressure] -> LoadAvg -> Curses ()
myRender window pressures loadA =
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
    moveCursor 2 30
    moveCursor 11 1
    drawString "load avg:"
    moveCursor 11 14
    drawString $ renderLoad loadA
    moveCursor 13 1
    drawString "(press q to quit)"
    moveCursor 0 0
