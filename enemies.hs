module Enemies where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import Debug.Trace
import Rendering
import DataTypes
import Helpers
enemyColor :: Color
enemyColor = blue

enemyObj1 :: Object
enemyObj1 = Object { position = (400, 250),
                     direction = (0, 0),
                     speed = 100,
                     boundingBox = (0,0),
                     graphic = color enemyColor $ rectangleSolid (70.0) (20.0)
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


changeDir:: Game -> (Float, Float) -> (Float, Float)
changeDir gameState@(GameState {enemy=eny@(Object {position = pos, direction = dir})})
   | fst(pos eny) <= 400 = (-1.0,0)
  {-| fst(position enemy) => 200 = (1.0,0)-}
   | snd(pos eny) <= 250 = (0,-1.0)
  {-| snd(position enemy) => -100 = (0,1.0)-}
           
updateEnemy dt gameState@(GameState {enemy=enemy}) = newEnemy
  where
    (dx,dy) = changeDir (position enemy) (direction enemy)
    enemySpeed = speed enemy
    v = (dx*enemySpeed*dt,dy*enemySpeed*dt)
    newEnemy = moveObject enemy v
    
