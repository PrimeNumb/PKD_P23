module Enemies where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import Debug.Trace
import Rendering
import DataTypes

enemyObj1 :: Object
enemyObj1 = Object { position = (230, 200),
                     direction = (0, 0),
                     speed = 100,
                     boundingBox = (0,0),
                     graphic = rectangleSolid 70.0 20.0
                   }

moveEnemy :: Game -> (Float, Float) -> Game
moveEnemy gameState@(GameState {enemy = enemy}) (ex, ey) = gameState {enemy = newEnemy}
  where
    (x, y) = position enemy
    (nx, ny) = (x+ex, y+ey)
    newEnemy = enemy {position = (nx, ny)}

updateEnemy dt gameState@(GameState {enemy=enemy}) = moveEnemy gameState v
  where
    (dx,dy) = direction enemy
    enemySpeed = speed enemy
    v = (dx*enemySpeed*dt,dy*enemySpeed*dt)
