module Enemies where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import Debug.Trace
import Rendering
import DataTypes
import Helpers
import Projectile
import Globals
import Collision

enemyDefaultSpawnPos :: Position
enemyDefaultSpawnPos = (win_width+enemy_width, 0)
  where
    enemy_width = fst $ boundingBox $ ship_obj enemyShipTemplate

enemyObjTemplate :: Object
enemyObjTemplate = Object { position = enemyDefaultSpawnPos,
                            direction = (-1, 0),
                            speed = 100,
                            boundingBox = (25, 49),
                            graphic = enemySprite
                          }

enemyShipTemplate :: Ship
enemyShipTemplate = Ship { ship_obj = enemyObjTemplate,
                           ship_health = 3,
                           wep_cooldown = 2.0,
                           projectile = enemyDefaultProj,
                           last_fired_tick = 0,
                           isPlayer = False,
                           isFiring = True
                         }

enemyColor :: Color
enemyColor = blue

enemyObj1 :: Object
enemyObj1 = Object { position = (400, 250),
                     direction = (-1.0, 0),
                     speed = 50,
                     boundingBox = (25,25),
                     graphic = color enemyColor $ rectangleSolid (50.0) (50.0)
                   }
                    
processDir :: Position -> Direction -> Direction
processDir (x,y) (dx,dy)
  | y < 0 = (dx, 1.0)
  | y > 100 = (dx, (-1.0))
  | otherwise = (dx, dy)


updateEnemy :: Float -> Game -> Ship -> Ship               
updateEnemy dt gameState@(GameState {ticker=currentTick}) enemy = newEnemy
  where
    -- Update the last fired tick
    canFire = (currentTick - (last_fired_tick enemy)) > (wep_cooldown enemy)
    updatedTick =
      case canFire of
        False -> last_fired_tick enemy
        True  -> currentTick
    enemyObj = ship_obj enemy
    --newEnemyObj = enemyMovement enemyObj
    newEnemyObj = enemyObj --if we need to change something in the obj, do that here
    -- Movement
    (dx,dy) = direction newEnemyObj
    enemySpeed = speed enemyObj
    deltaPos = (dx*enemySpeed*dt,dy*enemySpeed*dt)
    newEnemy = enemy { ship_obj = (move newEnemyObj deltaPos), last_fired_tick = updatedTick, isFiring=(not $ outOfBounds newEnemyObj) }
