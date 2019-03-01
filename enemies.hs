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


-- The default spawn position for enemy ships.
enemyDefaultSpawnPos :: Position
enemyDefaultSpawnPos = (winWidth+enemyWidth, 0)
  where
    enemyWidth = fst $ bounds $ shipObj enemyShipDefaultTemplate


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
{- updateEnemy deltaTime gameState ship1
   Updates an enemy one iteration.
   PRE: deltaTime >= 0
   RETURNS: A ship based on ship1 with updated properties based on deltaTime & gameState.
   EXAMPLES: updateEnemy 1.0 defaultGameState enemyShipDefaultTemplate == Ship {...}
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
    newEnemyObj = enemyObj
    -- Movement
    (dx,dy) = direction newEnemyObj
    enemySpeed = speed enemyObj
    deltaPos = (dx*enemySpeed*dt,dy*enemySpeed*dt)
    newEnemy = enemy { shipObj = (move deltaPos newEnemyObj), lastFiredTick = updatedTick, isFiring=(not $ outOfBounds newEnemyObj background) }
