module Collision where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import Debug.Trace
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
checkRectCollision obj1@(Object {position=p1@(x1, y1), boundingBox=box1@(r1x, r1y)}) obj2@(Object {position=p2@(x2, y2), boundingBox=box2@(r2x, r2y)}:xs) =
  if (r1x1 > r2x2 && r1x2 < r2x1  && r1y1 > r2y2 && r1y2 < r2y1)
  then True
  else checkRectCollision obj1 xs
  where
    r1x1 = x1 + r1x
    r1x2 = x1 - r1x
    r2x1 = x2 + r2x
    r2x2 = x2 - r2x
    r1y1 = y1 + r1y
    r1y2 = y1 - r1y
    r2y1 = y2 + r2y
    r2y2 = y2 - r2y

--TODO: Change enemy into enemies! We need more!
--TODO: Fix bunding boxes? It's a pretty wonky way to handle collision tbh...
--TODO: Make enemybullets and friendlybullets, they don't interact the same way.

{-
playerCollideShip :: Game -> Bool
playerCollideShip gameState@(Game {player=ply, enemy=enemies}) = checkRectCollision ply enemies 



playerCollideBullet :: Game -> Bool
playerCollideBullet = gameState@(Game {player=ply, npc_projectiles=proj}) = checkRectCollision ply proj

-}
colPlyProj :: Game -> [Projectile] -> [Projectile]
colPlyProj _ [] = []
colPlyProj gameState@(GameState {player=ply}) (x@(Projectile {proj_obj=obj}):xs) = if checkRectCollision (ship_obj ply) [obj] then colPlyProj gameState xs else x : colPlyProj gameState xs




{-
outOfBounds :: Object -> Bool
outOfBounds 
  
-}

-- Collisiontests

o1 :: Object
o1 = Object { position = (2, 3),
              direction = (0, 0),
              speed = 0,
              boundingBox = (1, 1),
              graphic = color green $ rectangleSolid 50.0 50.0
            }
o2 :: Object
o2 = Object { position = (3, 4),
              direction = (0, 0),
              speed = 0,
              boundingBox = (1, 1),
              graphic = color green $ rectangleSolid 50.0 50.0
            }
     
o3 :: Object
o3 = Object { position = (150, 200),
              direction = (0, 0),
              speed = 300,
              boundingBox = (10, 10),
              graphic = color green $ rectangleSolid 50.0 50.0
            }
     




