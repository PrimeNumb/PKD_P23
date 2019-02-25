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



enemyShipTemplate :: Ship
enemyShipTemplate = Ship { ship_obj = enemyObj1,
                           ship_health = 5,
                           wep_cooldown = 1.0,
                           projectile = testProj,
                           last_fired_tick = 0,
                           isPlayer = False
                         }

changeDir :: Object -> (Float, Float) -> Object
changeDir obj (x,y) = obj {direction = (x, y)}
                                      



enemyMovement :: Object -> Object
enemyMovement obj
  | snd(position obj) > 300.0 = trace traceStr $ changeDir obj (fst(direction obj),-1)
  | snd(position obj) < -100.0 = trace traceStr $ changeDir obj (fst(direction obj),1)
  | otherwise = trace traceStr obj
  where
    traceStr = (show $ snd (position obj))  ++ " " ++ (show c1) ++ " " ++ (show c2) ++ " " ++ (show $ snd (direction obj))
    c1 = snd(position obj) > 300
    c2 = snd(position obj) < 100
    {-ny = -}


--movementPattern = 

updateEnemy :: Float -> Game -> Ship                   
updateEnemy dt gameState@(GameState {enemy=enemy}) = newEnemy
  where
    enemyObj = ship_obj enemy
    (dx,dy) = direction $ enemyMovement enemyObj
    enemySpeed = speed enemyObj
    v = (dx*enemySpeed*dt,dy*enemySpeed*dt)
    newEnemyObj = moveObject enemyObj v
    newEnemy = enemy { ship_obj = newEnemyObj }

