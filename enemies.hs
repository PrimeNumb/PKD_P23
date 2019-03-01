module Enemies where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import Debug.Trace
import Rendering
import DataTypes
import Utilities
import Projectile
import Globals
import Collision


  --Gives a  standard enemy spawning position
enemyDefaultSpawnPos :: Position
enemyDefaultSpawnPos = (win_width+enemy_width, 0)
  where
    enemy_width = fst $ bounds $ ship_obj enemyShipDefaultTemplate


--Gives a standard enemy object
enemyObjTemplate :: Object
enemyObjTemplate = Object { position = enemyDefaultSpawnPos,
                            direction = (-1, 0),
                            speed = 100,
                            bounds = (25, 49),
                            graphic = color blue $ rectangleSolid 50 98
                          }

--The enemy template that decide the stats of the enemy
enemyShipDefaultTemplate :: Ship
enemyShipDefaultTemplate = Ship { ship_obj = enemyObjTemplate,
                           ship_health = 3,
                           wep_cooldown = 2.0,
                           projectile = enemyDefaultProj,
                           last_fired_tick = 0,
                           isPlayer = False,
                           isFiring = True
                         }
-- Enemy color
enemyColor :: Color
enemyColor = blue

-- Another type of enemy
enemyObj1 :: Object
enemyObj1 = Object { position = (400, 250),
                     direction = (-1.0, 0),
                     speed = 50,
                     bounds = (25,25),
                     graphic = color enemyColor $ rectangleSolid (50.0) (50.0)
                   }


  {- processDir
     Takes a position and a direction and gives a new direction
     RETURNS: A direction
     EXAMPLES: processDir (200, 200) (-1, 1) = 
  -}
  
processDir :: Position -> Direction -> Direction
processDir (x,y) (dx,dy)
  | y < 0 = (dx, 1.0)
  | y > 100 = (dx, (-1.0))
  | otherwise = (dx, dy)



  {- updateEnemy time gamestate enemyship
     Takes the time until next tick, the current game state and an enemy ship and returns an updated version of the given ship
     RETURNS: An updated version of the given ship
     EXAMPLES: ...
  -}
updateEnemy :: Float -> Game -> Ship -> Ship               
updateEnemy dt gameState@(GameState {ticker=currentTick,background=background}) enemy = newEnemy
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
    newEnemy = enemy { ship_obj = (move newEnemyObj deltaPos), last_fired_tick = updatedTick, isFiring=(not $ outOfBounds newEnemyObj background) }
