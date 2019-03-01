module Globals where
import Graphics.Gloss.Juicy
import Graphics.Gloss
import DataTypes
import System.IO.Unsafe

-- Should these be kept in main?
winTitle :: String
winTitle = "Space Shooter"

-- CHANGE WINDOW SIZE LATER BEFORE DEMO
winWidth :: Float
winWidth = 1024.0

winHeight :: Float
winHeight = 576.0

winSize :: (Int, Int)
winSize = (floor winWidth, floor winHeight)

winOffset :: (Int, Int)
winOffset = (0, 0)

winBackground :: Color
winBackground = black

targetFramerate :: Int
targetFramerate = 60

enemySpawnInterval :: Float
enemySpawnInterval = 2.0

enemySpawnInitialDelay :: Float
enemySpawnInitialDelay = 5.0

--Sprites
--function png taken from gloss-game (credit the author?)

-- D
png :: FilePath -> Picture
png fname = maybe (text "PNG ERROR") id (unsafePerformIO $ loadJuicyPNG fname)

playerSpritePath :: FilePath
playerSpritePath = "./sprites/player2.png"

enemySpritePath :: FilePath
enemySpritePath = "./sprites/enemyShip.png"

plyProjSpritePath :: FilePath
plyProjSpritePath = "./sprites/laserRed.png"

enemyProjSpritePath :: FilePath
enemyProjSpritePath = "./sprites/laserGreen.png"

heartSpritePath :: FilePath
heartSpritePath = "./sprites/heart.png"

gameOverSprite :: Picture
gameOverSprite = png "./sprites/gameOver.png"

gameOverSpritePath :: FilePath
gameOverSpritePath = "./sprites/gameOver.png"

backgroundPath :: FilePath
backgroundPath = "./sprites/spacebackg.png"

----------------------------------------------------------------------------

defaultBackground :: Object
defaultBackground =
  Object { position = (0, 0),
           direction = (0, 0),
           speed = 0,
           bounds = (winWidth/2, winHeight/2),
           graphic = Blank
         }

