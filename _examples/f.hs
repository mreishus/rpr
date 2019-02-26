ncursesInit :: Editor -> IO () -- what is this () thing
ncursesInit ed = unless (null $ filename ed) $ runCurses $ do
  setEcho False
  w <- defaultWindow
  updateWindow w $ do
    moveCursor 0 0
    drawString (text ed)
    moveCursor 0 (toInteger (cursor ed))
    drawString "(press q to quit)"
    moveCursor 0 0
    render
    ev <- getEvent w Nothing
    case ev of
       Nothing -> ncursesInit ed
       Just ev'
         | ev' == EventCharacter 'q' -> ncursesInit (cursorForward ed)
         | otherwise                 -> ncursesInit (Editor "" False 0 0 False "" "")