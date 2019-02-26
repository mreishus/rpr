module Main where

import           Control.Monad          (forM_)
import           Control.Monad.IO.Class
import           Debug.Trace            (trace)
import           GHC.Int
import           Lib
import           Parse                  (Pressure, PressureType (..), avg10,
                                         avg300, avg60, getString, parseCpu,
                                         parseIO, parseMemory, pressure_of)
import           UI.NCurses

num_accross = 24

num_down = 9

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

renderPressures :: IO [Pressure] -> IO String
renderPressures pressures = do
  a <- pressures
  let b = filter (\x -> pressure_of x == CpuPressure) a
  let c = show $ avg10 $ head b
  return c

main :: IO ()
main =
  runCurses $ do
    setEcho False
    w <- defaultWindow
    pressures <- liftIO getPressures
    myRender w pressures
    render
    waitFor w (\ev -> ev == EventCharacter 'q' || ev == EventCharacter 'Q')

renderCpuPressure :: [Pressure] -> String
renderCpuPressure ps = a10 ++ " " ++ a60 ++ " " ++ a300
  where
    c = head $ filter (\x -> pressure_of x == CpuPressure) ps
    a10 = show $ avg10 c
    a60 = show $ avg60 c
    a300 = show $ avg300 c

myRender :: Window -> [Pressure] -> Curses ()
myRender window pressures =
  updateWindow window $ do
    moveCursor 1 10
    drawString "Hello world!"
    moveCursor 2 10
    drawString $ renderCpuPressure pressures
    --drawString pressure
    moveCursor 3 10
    drawString "(press q to quit)"
    moveCursor 0 0

waitFor :: Window -> (Event -> Bool) -> Curses ()
waitFor w p = loop
  where
    loop = do
      ev <- getEvent w Nothing
      case ev of
        Nothing -> loop
        Just ev' ->
          if p ev'
            then return ()
            else loop
