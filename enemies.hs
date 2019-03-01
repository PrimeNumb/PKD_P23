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


--Gives a default enemy spawning position
enemyDefaultSpawnPos :: Position
enemyDefaultSpawnPos = (winWidth+enemyWidth, 0)
  where
    enemyWidth = fst $ bounds $ shipObj enemyShipDefaultTemplate


--An enemy object

enemyObjTemplate :: Object
enemyObjTemplate = Object { position = enemyDefaultSpawnPos,
                            direction = (-1, 0),
                            speed = 100,
                            bounds = (25, 49),
                            graphic = color blue $ rectangleSolid 50 98
                          }
-- An enemy object

enemyObj1 :: Object
enemyObj1 = Object { position = (400, 250),
                     direction = (-1.0, 0),
                     speed = 50,
                     bounds = (25,25),
                     graphic = color enemyColor $ rectangleSolid (50.0) (50.0)
                   }


--The enemy ships' default template that gives the object, health, weapon cooldown, projectile, last fired tick, information regarding if the ship is the player and if the ship is firing
enemyShipDefaultTemplate :: Ship
enemyShipDefaultTemplate = Ship { shipObj = enemyObjTemplate,
                           shipHealth = 3,
                           wepCooldown = 2.0,
                           projectile = enemyDefaultProj,
                           lastFiredTick = 0,
                           isPlayer = False,
                           isFiring = True
                         }
-- Enemy color
enemyColor :: Color
enemyColor = blue


  {- updateEnemy time gamestate enemyship
     Takes the time since the last tick, the current game state and an enemy ship and returns an      updated version of the given ship
     PRE: True
     RETURNS: An updated version of the given ship with an updated tick, position and information     regarding if its firing or not
     EXAMPLES: ...
  -}
updateEnemy :: Float -> Game -> Ship -> Ship               
updateEnemy dt gameState@(GameState {ticker=currentTick,background=background}) enemy = newEnemy
  where
    -- Update the last fired tick
    canFire = (currentTick - (lastFiredTick enemy)) > (wepCooldown enemy)
    updatedTick =
      case canFire of
        False -> lastFiredTick enemy
        True  -> currentTick
    enemyObj = shipObj enemy
    --newEnemyObj = enemyMovement enemyObj
    newEnemyObj = enemyObj
    -- Movement
    (dx,dy) = direction newEnemyObj
    enemySpeed = speed enemyObj
    deltaPos = (dx*enemySpeed*dt,dy*enemySpeed*dt)
    newEnemy = enemy { ship_obj = (move newEnemyObj deltaPos), last_fired_tick = updatedTick, isFiring=(not $ outOfBounds newEnemyObj background) }


--Function that changes direction, saved for possible future use

{-
processDir :: Position -> Direction -> Direction
processDir (x,y) (dx,dy)
  | y < 0 = (dx, 1.0)
  | y > 100 = (dx, (-1.0))
  | otherwise = (dx, dy)
-}
