module Globals where
import Graphics.Gloss.Juicy
import Graphics.Gloss
import DataTypes
import System.IO.Unsafe

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


--Sprites
--function png taken from gloss-game (credit the author?)

png :: FilePath -> Picture
png fname = maybe (text "PNG ERROR") id (unsafePerformIO $ loadJuicyPNG fname)

playerSprite :: Picture
playerSprite = png "./player2.png"

enemySprite :: Picture
enemySprite = png "./enemyShip.png"

plyProjSprite :: Picture
plyProjSprite = png "laserRed.png"

npcProjSprite :: Picture
npcProjSprite = png "laserGreen.png"

background :: Object
background =
  Object { position = (0, 0),
           direction = (0, 0),
           speed = 0,
           boundingBox = (512, 384),
           graphic = png "./spacebackg2.png"
         }


