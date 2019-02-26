module Main where

import           Control.Monad.State
import           UI.NCurses

main :: IO ()
main = runCurses $ void (runStateT top [])

top = do
  lift $ setEcho False
  w <- lift $ defaultWindow
  lift getEvent w $ Just 1000
-- lift $ getEvent w $ Just 1000
