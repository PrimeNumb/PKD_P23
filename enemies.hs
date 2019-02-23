module Enemies where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import Debug.Trace
import Rendering
import DataTypes
enemyColor :: Color
enemyColor = blue

enemyObj1 :: Object
enemyObj1 = Object { position = (400, 250),
                     direction = (0, 1.0),
                     speed = 100,
                     boundingBox = ((0, 0), (0, 0)),
                     graphic = color enemyColor $ rectangleSolid (70.0) (20.0)
                   }

--getEnemyPos :: Game -> (Float, Float)
--getEnemyPos gameState = position $ enemy gameState


--movePatternEnemy :: (Float, Float)
--movePatternEnemy (ex, ey


moveEnemy :: Game -> (Float, Float) -> Game
moveEnemy gameState@(GameState {enemy = enemy}) (ex, ey) = gameState {enemy = newEnemy}
  where
    (x, y) = position enemy
    (nx, ny) = (x, y+ey)
    newEnemy = enemy {position = (nx, ny)}

updateEnemy dt gameState@(GameState {enemy=enemy}) = moveEnemy gameState v
  where
    (dx,dy) = direction enemy
    enemySpeed = speed enemy
    v = (dx*enemySpeed*dt,dy*enemySpeed*dt)

