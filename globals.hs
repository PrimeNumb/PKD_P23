module Globals where
import Graphics.Gloss

-- DO NOT CHANGE THESE OUTSIDE THIS MODULE
win_title :: String
win_title = "Space Shooter"

win_size :: (Int, Int)
win_size = (640, 480)

win_offset :: (Int, Int)
win_offset = (0, 0)

win_background :: Color
win_background = white

targetFramerate :: Int
targetFramerate = 60
