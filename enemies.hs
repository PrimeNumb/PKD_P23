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

enemyObj1 :: Object
enemyObj1 = Object { position = (400, 250),
                     direction = (-1.0, -1.0),
                     speed = 50,
                     boundingBox = (35,10),
                     graphic = color enemyColor $ rectangleSolid (70.0) (20.0)
                   }

{-
updateEnemies :: Game -> [Ship] -> [Ship]
updateEnemies _ [] = []
updateEnemies gameState@(GameState {ply_projectiles=proj}) (ship:xs) =
  if health <= 0 then []
  else newShip : updateEnemies gameState xs
  where
    newShip = applyEffect (getEffect ship proj) ship
-}

enemyShipTemplate :: Ship
enemyShipTemplate = Ship { ship_obj = enemyObj1,
                           ship_health = 5,
                           wep_cooldown = 1.0,
                           projectile = testProj,
                           last_fired_tick = 0,
                           isFiring = False,
                           isPlayer = False
                         }


changeDir :: Object -> (Float, Float) -> Object
changeDir obj (x,y) = obj {direction = (x, y)}
                                      



enemyMovement :: Object -> Object
enemyMovement enemy = changeDir enemy (fst(direction enemy), ny)
  where
    ny
      | snd(position enemy) > 300.0  = -1.0
      | snd(position enemy) < -100.0 = 1.0
      | otherwise = snd(direction enemy)



updateEnemy :: Float -> Game -> Ship                   
updateEnemy dt gameState@(GameState {enemy=enemy}) = newEnemy
  where
    enemyObj = ship_obj enemy
    (dx,dy) = direction $ enemyMovement enemyObj
    enemySpeed = speed enemyObj
    v = (dx*enemySpeed*dt,dy*enemySpeed*dt)
    newEnemyObj = moveObject enemyObj v
    newEnemy = enemy { ship_obj = newEnemyObj }

