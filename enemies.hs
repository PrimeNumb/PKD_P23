module Enemies where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import Debug.Trace
import Rendering
import DataTypes
import Helpers
import Projectile


enemyColor :: Color
enemyColor = blue

enemyDefaultProjObj =
  Object { position = (0,0),
           direction = (-1,0),
           speed = projObjDefault_spd,
           boundingBox = projObjDefault_bbox,
           graphic = projObjDefault_gfx
         }
enemyDefaultProj = Projectile enemyDefaultProjObj (Damage 1)


enemyObj1 :: Object
enemyObj1 = Object { position = (400, 250),
                     direction = (-1.0, 0),
                     speed = 50,
                     boundingBox = (25,25),
                     graphic = color enemyColor $ rectangleSolid (50.0) (50.0)
                   }
                    
enemyMovement :: Object -> Object
enemyMovement enemy = changeDir enemy (fst(direction enemy), ny)
  where
    ny
      | snd(position enemy) > 300.0  = -1.0
      | snd(position enemy) < -100.0 = 1.0
      | otherwise = snd(direction enemy)

{-
updateEnemies :: Game -> [Ship] -> [Ship]
updateEnemies _ [] = []
updateEnemies gameState@(GameState {ply_projectiles=proj}) (ship:xs) =
  if health <= 0 then []
  else newShip : updateEnemies gameState xs
  where
    newShip = applyEffect (getEffect ship proj) ship
-}

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
    newEnemy = enemy { ship_obj = (moveObject newEnemyObj deltaPos), last_fired_tick = updatedTick }
