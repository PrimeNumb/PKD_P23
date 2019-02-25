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
                     boundingBox = (25,25),
                     graphic = color enemyColor $ rectangleSolid (50.0) (50.0)
                   }

enemyShipTemplate :: Ship
enemyShipTemplate = Ship { ship_obj = enemyObj1,
                           ship_health = 5,
                           wep_cooldown = 1.0,
                           projectile = testProj,
                           last_fired_tick = 0,
                           isFiring = False,
                           isPlayer = False
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


--processDir :: Position -> Direction -> Direction
--processDir (x,y) (dx,dy)
--  | y < 0 = (dx, 1.0)
--  | y > 100 = (dx, (-1.0))
--  | otherwise = (dx, dy)

--updateEnemy :: Float -> Game -> Ship
--updateEnemy dt gameState@(GameState {enemy=oldEnemy}) = trace traceStr $ newEnemy
--  where
--    enemyObj = ship_obj oldEnemy
--    enemyPos = position enemyObj
--    (dx, dy) = processDir enemyPos $ direction enemyObj
--    pos_y = snd enemyPos 
--    enemySpeed = (speed enemyObj)
--    deltaMove = (dx*enemySpeed*dt,dy*enemySpeed*dt)
--    --traceStr = "Tick: " ++ show (ticker gameState) ++  show (dx, dy)
--    traceStr = show enemySpeed
--    newEnemyObj = enemyObj { direction = (dx, dy)}
--    newEnemy = oldEnemy { ship_obj = (moveObject newEnemyObj deltaMove)}
--    --traceStr = show $ direction $ ship_obj newEnemy

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
    --newEnemyObj = enemyMovement enemyObj
    newEnemyObj = enemyObj --if we need to change something in the obj, do that here
    (dx,dy) = direction newEnemyObj
    enemySpeed = speed enemyObj
    v = (dx*enemySpeed*dt,dy*enemySpeed*dt)
    newEnemy = enemy { ship_obj = (moveObject newEnemyObj v) }

