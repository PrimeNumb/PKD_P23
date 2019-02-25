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

changeDir :: Object -> (Float, Float) -> Object
changeDir obj (x,y) = obj {direction = (x, y)}
                                      



--enemyMovement :: Object -> Object
--enemyMovement enemy = changeDir enemy (fst(direction enemy), ny)
--  where
--    ny
--      | snd(position enemy) > 300.0  = -1.0
--      | snd(position enemy) < -100.0 = 1.0
--      | otherwise = snd(direction enemy)

processDir :: Position -> Direction -> Direction
processDir (x,y) (dx,dy)
  | y < 0 = (dx, 1.0)
  | y > 100 = (dx, (-1.0))
  | otherwise = (dx, dy)
--  where
    --traceStr1 = "y = " ++ show y ++ "\n"
    --traceStr2 = show y ++ " < 0 = " ++ (show $ y<0) ++ "\n"
    --traceStr3 = show y ++ " and " ++ show y ++ " > 100 = " ++ (show $ y>100) ++ "\n"
    --traceStr4 = "(dx,dy) = " ++ (show (dx,dy)) ++ "\n"
    --traceStr1st = "Changing dy to 1.0"
    --traceStr2nd = "Changing dy to -1.0"
    --traceStr3rd = "Letting dy = " ++ show dy
    --traceStr = traceStr1 ++ traceStr2 ++ traceStr3 ++ traceStr4
updateEnemy :: Float -> Game -> Ship
updateEnemy dt gameState@(GameState {enemy=oldEnemy}) = trace traceStr $ newEnemy
  where
    enemyObj = ship_obj oldEnemy
    enemyPos = position enemyObj
    (dx, dy) = processDir enemyPos $ direction enemyObj
    pos_y = snd enemyPos 
    enemySpeed = speed enemyObj
    deltaMove = (dx*enemySpeed*dt,dy*enemySpeed*dt)
    --traceStr = "Tick: " ++ show (ticker gameState) ++  show (dx, dy)
    newEnemy = oldEnemy { ship_obj = (moveObject enemyObj deltaMove)}
    --traceStr = show $ direction $ ship_obj newEnemy

--shouldChangeDir :: Direction -> Bool
--shouldChangeDir (x,y)
--  | y < 0 = True
--  | y > 100 = True
--  | otherwise = False
-- 
--modEnemyDir :: Float -> Game -> Direction -> Game
--modEnemyDir dt gameState (dx,dy) = gameState
--  where
--updateEnemy :: Float -> Game -> Ship
--updateEnemy dt gameState@(GameState {enemy=oldEnemy}) = trace traceStr $ newEnemy
--  where
--    enemyObj = ship_obj oldEnemy
--    enemyPos = position enemyObj
--    (dx, dy) = processDir enemyPos $ direction enemyObj
--    pos_y = snd enemyPos 
--    enemySpeed = speed enemyObj
--    deltaMove = (dx*enemySpeed*dt,dy*enemySpeed*dt)
--    traceStr = "updateEnemy: " ++ show (dx, dy)
--    newEnemy = oldEnemy { ship_obj = (moveObject enemyObj deltaMove)}


--updateEnemy :: Float -> Game -> Ship                   
--updateEnemy dt gameState@(GameState {enemy=enemy}) = newEnemy
--  where
--    enemyObj = ship_obj enemy
--    (dx,dy) = direction $ enemyMovement enemyObj
--    enemySpeed = speed enemyObj
--    v = (dx*enemySpeed*dt,dy*enemySpeed*dt)
--    newEnemyObj = moveObject enemyObj v
--    newEnemy = enemy { ship_obj = newEnemyObj }

