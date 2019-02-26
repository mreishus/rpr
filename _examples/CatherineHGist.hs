import           Control.Monad (forM_)
import           Data.Int
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

main =
  runCurses $ do
    w <- defaultWindow
    forM_ [(x, y) | x <- [0 .. num_accross], y <- [0 .. num_down]] $ \(x, y) -> do
      let colIndex = x * (num_down + 1) + y + 1
      let my_color = Color (fromIntegral colIndex :: Int16)
      defineColor my_color (x * 36) 0 (y * 110)
      cid <- newColorID my_color ColorBlack (toInteger colIndex)
      updateWindow w $ do
        setColor cid
        moveCursor (fromIntegral y) (fromIntegral x)
        drawString "■"
    render
    waitFor w (\ev -> ev == EventCharacter 'q' || ev == EventCharacter 'Q')

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
