module Collision where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import Debug.Trace
import Enemies
import Player
import Projectile
import DataTypes
import Globals
import Rendering
import Helpers



{- checkRectCollision
Checks is two objects overlap (collide).
PRE: The object's bounding boxes have their top left corner listed as the first 2-tuple. 
RETURNS:
EXAMPLES: 
-}

checkRectCollision :: Object -> [Object] -> Bool
checkRectCollision _ [] = False
checkRectCollision obj1@(Object {position=p1@(x1, y1), boundingBox=box1@((box1x1, box1y1), (box1x2, box1y2))}) obj2@(Object {position=p2@(x2, y2), boundingBox=box2@((box2x1, box2y1), (box2x2, box2y2))}:xs) =
  if (r1x1 > r2x2 && r1x2 < r2x1  && r1y1 > r2y2 && r1y2 < r2y1) then True else checkRectCollision obj1 xs
  where
    r1x1 = box1x1 + x1
    r1x2 = box1x2 + x1
    r2x1 = box2x1 + x2
    r2x2 = box2x2 + x2
    r1y1 = box1y1 + y1
    r1y2 = box1y2 + y1
    r2y1 = box2y1 + y2
    r2y2 = box2y2 + y2



--TODO: Change enemy into enemies! We need more!
--TODO: Fix bunding boxes? It's a pretty wonky way to handle collision tbh...
--TODO: Make enemybullets and friendlybullets, they don't interact the same way.

{-
playerCollideShip :: Game -> Bool
playerCollideShip gameState@(Game {player=ply, enemy=enemies}) = checkRectCollision ply enemies 



playerCollideBullet :: Game -> Bool
playerCollideBullet = gameState@(Game {player=ply, projectiles=proj}) = checkRectCollision ply proj


--This one is harder, use foldl?
enemyCollideBullet :: Game -> Bool
enemyCollideBullet = undefined


-- Collisiontests
o1 :: Object
o1 = Object { position = (200, 200),
              direction = (0, 0),
              speed = 300,
              boundingBox = ((25, 25), (-25, -25)),
              graphic = color green $ rectangleSolid 50.0 50.0
            }
o2 :: Object
o2 = Object { position = (-200, -200),
              direction = (0, 0),
              speed = 300,
              boundingBox = ((25, 25), (-25, -25)),
              graphic = color green $ rectangleSolid 50.0 50.0
            }
     
-}




