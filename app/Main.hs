module Main where

import           Control.Monad.IO.Class
import           Lib
import           UI.NCurses

--readFile  :: FilePath -> IO String
getCpuPressure = readFile "/proc/pressure/cpu"

main :: IO ()
main =
  runCurses $ do
    setEcho False
    w <- defaultWindow
    pressure <- liftIO getCpuPressure
    myRender w pressure
    render
    waitFor w (\ev -> ev == EventCharacter 'q' || ev == EventCharacter 'Q')

myRender :: Window -> String -> Curses ()
myRender window pressure =
  updateWindow window $ do
    moveCursor 1 10
    drawString "Hello world!"
    drawString pressure
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
