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
                     direction = (-1.0, 0),
                     speed = 100,
                     boundingBox = (0,0),
                     graphic = color enemyColor $ rectangleSolid (70.0) (20.0)
                   }
enemyShipTemplate :: Ship
enemyShipTemplate = Ship { ship_obj = enemyObj1,
                           ship_health = 100,
                           wep_cooldown = 1.0,
                           projectile = testProj,
                           last_fired_tick = 0,
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

updateEnemy :: Float -> Game -> Ship                   
updateEnemy dt gameState@(GameState {enemy=enemy}) = newEnemy
  where
    enemyObj = ship_obj enemy
    (dx,dy) = direction enemyObj
    enemySpeed = speed enemyObj
    v = (dx*enemySpeed*dt,dy*enemySpeed*dt)
    newEnemyObj = moveObject enemyObj v
    newEnemy = enemy { ship_obj = newEnemyObj }
