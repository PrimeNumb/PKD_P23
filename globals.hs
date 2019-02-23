module Globals where
import Graphics.Gloss

-- Should these be kept in main?
win_title :: String
win_title = "Space Shooter"

win_size :: (Int, Int)
win_size = (1024, 768)

win_offset :: (Int, Int)
win_offset = (0, 0)

win_background :: Color
win_background = black

targetFramerate :: Int
targetFramerate = 60
