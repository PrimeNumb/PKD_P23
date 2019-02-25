module Globals where
import Graphics.Gloss
import DataTypes

-- Should these be kept in main?
win_title :: String
win_title = "Space Shooter"

win_size :: (Int, Int)
win_size = (1024, 768)


border :: Object
border =
  Object { position = (0, 0),
           direction = (0, 0),
           speed = 0,
           boundingBox = (512, 384),
           graphic = Blank
         }

win_offset :: (Int, Int)
win_offset = (0, 0)

win_background :: Color
win_background = black

targetFramerate :: Int
targetFramerate = 60
