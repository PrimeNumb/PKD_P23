module Globals where
import Graphics.Gloss.Juicy
import Graphics.Gloss
import DataTypes
import System.IO.Unsafe

-- Should these be kept in main?
win_title :: String
win_title = "Space Shooter"

-- CHANGE WINDOW SIZE LATER BEFORE DEMO
win_width :: Float
win_width = 1024.0

win_height :: Float
win_height = 576.0

win_size :: (Int, Int)
win_size = (floor win_width, floor win_height)

win_offset :: (Int, Int)
win_offset = (0, 0)

win_background :: Color
win_background = black

targetFramerate :: Int
targetFramerate = 60

enemy_spawn_interval :: Float
enemy_spawn_interval = 2.0

enemy_spawn_initial_delay :: Float
enemy_spawn_initial_delay = 5.0

--Sprites
--function png taken from gloss-game (credit the author?)

png :: FilePath -> Picture
png fname = maybe (text "PNG ERROR") id (unsafePerformIO $ loadJuicyPNG fname)

playerSprite :: Picture
playerSprite = png "./sprites/player2.png"

enemySprite :: Picture
enemySprite = png "./sprites/enemyShip.png"

plyProjSprite :: Picture
plyProjSprite = png "./sprites/laserRed.png"

npcProjSprite :: Picture
npcProjSprite = png "./sprites/laserGreen.png"

background :: Object
background =
  Object { position = (0, 0),
           direction = (0, 0),
           speed = 0,
           boundingBox = (win_width/2, win_height/2),
           graphic = png "./sprites/spacebackg.png"
         }


