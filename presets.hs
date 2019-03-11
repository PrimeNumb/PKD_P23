module Presets where
import Graphics.Gloss
import DataTypes
import System.Random


-- Window bindings
winTitle :: String
winTitle = "Space Shooter"

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

backgroundScrollSpeed :: Float
backgroundScrollSpeed = 200

----------------------------------------------------------------------------
-- Filepaths to sprites
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

gameOverSpritePath :: FilePath
gameOverSpritePath = "./sprites/gameOver.png"

winScreenSpritePath :: FilePath
winScreenSpritePath = "./sprites/winScreen.png"

backgroundPath :: FilePath
backgroundPath = "./sprites/spacebackg.png"

----------------------------------------------------------------------------
-- Player bindings
--The default player object
playerDefaultObj :: Object
playerDefaultObj = Object { position = (0, 0),
                     direction = (0, 0),
                     speed = 300,
                     bounds = (25, 25),
                     graphic = color green $ rectangleSolid 50 50
                   }

-- The default player ship
playerDefaultShip :: Ship
playerDefaultShip = Ship { shipObj = playerDefaultObj,
                    shipHealth = 3,
                    wepCooldown = 0.25,
                    projectile = playerDefaultProj,
                    lastFiredTick = 0,
                    isFiring = False,
                    isPlayer = True
                  }

--The default player projectile object
playerDefaultProjObj =
  Object { position = (0,0),
           direction = (1,0),
           speed = projObjDefaultSpeed,
           bounds = (16.5,4.5),
           graphic = projObjDefaultGfx
         }

--The default player projectile damage
playerDefaultProj = Projectile playerDefaultProjObj (Damage 1)

----------------------------------------------------------------------------
-- Enemy bindings

-- The default template for an enemy object
enemyObjTemplate :: Object
enemyObjTemplate = Object { position = enemyDefaultSpawnPos,
                            direction = (-1, 0),
                            speed = 100,
                            bounds = (25, 49),
                            graphic = color blue $ rectangleSolid 50 98
                          }

-- The default template for an enemy ship.
enemyShipDefaultTemplate :: Ship
enemyShipDefaultTemplate = Ship { shipObj = enemyObjTemplate,
                           shipHealth = 3,
                           wepCooldown = 2.0,
                           projectile = enemyDefaultProj,
                           lastFiredTick = 0,
                           isPlayer = False,
                           isFiring = True
                         }

--The default enemy projectile object
enemyDefaultProjObj =
  Object { position = (0,0),
           direction = (-1,0),
           speed = projObjDefaultSpeed,
           bounds = (16.5,4.5),
           graphic = projObjDefaultGfx
         }

--The default enemy projectile
enemyDefaultProj = Projectile enemyDefaultProjObj (Damage 1)

-- The default spawn position for enemy ships.
enemyDefaultSpawnPos :: Position
enemyDefaultSpawnPos = (winWidth+enemyWidth, 0)
  where
    enemyWidth = fst $ bounds $ shipObj enemyShipDefaultTemplate

-- Enemy spawn interval, how often enemies should spawn in seconds
enemySpawnInterval :: Float
enemySpawnInterval = 2.0

-- Enemy spawn delay, the initial delay (in seconds) until enemies start spawning
enemySpawnInitialDelay :: Float
enemySpawnInitialDelay = 5.0

-- The amount of enemies per game
enemySpawnAmount :: Int
enemySpawnAmount = 10

----------------------------------------------------------------------------
-- Projectile bindings

-- The default projectile speed
projObjDefaultSpeed :: Float
projObjDefaultSpeed = 600

--The default projectile bounds
projObjDefaultBounds :: Bounds
projObjDefaultBounds = (2.5,2.5)

--The default projectile graphic
projObjDefaultGfx :: Picture
projObjDefaultGfx = color red $ circleSolid 5

--The post-game ship (invisShip) projectile object
harmlessProjObj = Object { position = (0,0),
                        direction = (-1,0),
                        speed = 0,
                        bounds = (0,0),
                        graphic = rectangleSolid 1 1

                         }

--The post-game ship (invisShip) projectile
harmlessProj = Projectile harmlessProjObj (Damage 0)

----------------------------------------------------------------------------
-- Misc bindings

-- The default background object
defaultBackground :: Object
defaultBackground =
  Object { position = (0, 0),
           direction = (0, 0),
           speed = 0,
           bounds = ((winWidth-1)/2, (winHeight-1)/2),
           graphic = Blank
         }


--The default encounters

defaultEncounter = Encounter
  { popInterval = enemySpawnInterval,
    lastPop = enemySpawnInitialDelay,
    shipStack = []
  }

--The default graphics before sprites
defaultGameGfx = GameGfx
  {
    playerGfx = color green $ rectangleSolid 50 50,
    enemyStandardGfx = color green $ rectangleSolid 50 50,
    playerProjGfx = color green $ rectangleSolid 50 50,
    enemyProjGfx = color green $ rectangleSolid 50 50,
    heartGfx = color green $ rectangleSolid 50 50,
    gameOverGfx = color green $ rectangleSolid 50 50,
    winScreenGfx = color green $ rectangleSolid 50 50,
    backgroundGfx = color green $ rectangleSolid 50 50
  }

-- The initial game state
defaultGameState :: Game
defaultGameState = GameState {
  gameGfx = defaultGameGfx,
  enemies = [],
  randomGen = mkStdGen 1234,
  encounter = defaultEncounter,
  player = playerDefaultShip,
  plyProjectiles = [],
  enemyProjectiles = [],
  ticker = 0,
  background = defaultBackground,
  backgroundFx = defaultBackground,
  plyTemplate = playerDefaultShip,
  enemyTemplate = enemyShipDefaultTemplate,
  plyProjTemplate = playerDefaultProj,
  enemyProjTemplate = enemyDefaultProj,
  showHitbox  = False,
  isPaused    = False
  }

-- Dummy object, more or less a template
dummyObject :: Object
dummyObject = Object { position = (0,0),
                       direction = (0,0),
                       speed = 0,
                       bounds = (0,0),
                       graphic = Blank
                       }
                       

--GAME OVER OBJECTS
--The object assigned to the player while the game is over.
gameOverObject :: Object
gameOverObject = Object { position = (1000, 1000),
                          direction = (0, 0),
                          speed = 300,
                          bounds = (0, 0),
                          graphic = Blank
                        }
--The player becomes an invisible ship while the game is over.                
invisPlayer :: Ship
invisPlayer = Ship { shipObj = gameOverObject,
                     shipHealth = -1,
                     wepCooldown = 200.0,
                     projectile = harmlessProj,
                     lastFiredTick = 0,
                     isPlayer = False,
                     isFiring = False
                   }
