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
                     direction = (0, 0),
                     speed = 100,
                     boundingBox = (35,10),
                     graphic = color enemyColor $ rectangleSolid (70.0) (20.0)
                   }
enemyShipTemplate :: Ship
enemyShipTemplate = Ship { ship_obj = enemyObj1,
                           ship_health = 100,
                           wep_cooldown = 1.0,
                           projectile = testProj,
                           last_fired_tick = 0,
                           isFiring = False,
                           isPlayer = False
                         }

--getEnemyPos :: Game -> (Float, Float)
--getEnemyPos gameState = position $ enemy gameState


--movePatternEnemy :: (Float, Float)
--movePatternEnemy (ex, ey


{- moveEnemy :: Game -> (Float, Float) -> Game
moveEnemy gs@(GameState {enemy=eny}) (ex, ey) = gs {enemy = newEnemy}
  where
    (x, y) = position enemy
    (nx, ny) = (x+ex, y+ey)
    newEnemy = eny {position = (nx, ny)} -}


{-changeDir:: Game -> (Float, Float) -> (Float, Float)
changeDir gameState@(GameState {enemy=eny@(Object {position = pos, direction = dir})})
   | fst(pos eny) <= 400 = (-1.0,0)
  | fst(position enemy) => 200 = (1.0,0)
   | snd(pos eny) <= 250 = (0,-1.0)
  | snd(position enemy) => -100 = (0,1.0) -}

changeDir :: Object -> (Float, Float) -> Object
changeDir obj (x,y) = obj {direction = (x, y)}

updateEnemy :: Float -> Game -> Ship                   
updateEnemy dt gameState@(GameState {enemy=enemy}) = newEnemy
  where
    enemyObj = ship_obj enemy
    (dx,dy) = direction enemyObj
    enemySpeed = speed enemyObj
    v = (dx*enemySpeed*dt,dy*enemySpeed*dt)
    newEnemyObj = moveObject enemyObj v
    newEnemy = enemy { ship_obj = newEnemyObj }

