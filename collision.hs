module Collision where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import DataTypes
import Globals
import Enemies
import Debug.Trace
import Projectile


{- checkRectCollision
Checks is two objects overlap (collide).
PRE: The object's bounding boxes have their top left corner listed as the first 2-tuple. 
RETURNS:
EXAMPLES: 
-}

checkRectCollision :: Object -> Object -> Bool
checkRectCollision obj1@(Object {position=p1@(x1, y1), boundingBox=box1@(r1x, r1y)}) obj2@(Object {position=p2@(x2, y2), boundingBox=box2@(r2x, r2y)}) =
  if (r1x1 > r2x2 && r1x2 < r2x1  && r1y1 > r2y2 && r1y2 < r2y1)
  then True
  else False
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


playerCollideShip :: Game -> Ship -> Bool
playerCollideShip gameState@(GameState {player=ply}) ship = checkRectCollision (ship_obj ply) (ship_obj ship)


{-
playerCollideBullet :: Game -> Bool
playerCollideBullet = gameState@(Game {player=ply, npc_projectiles=proj}) = checkRectCollision ply proj
-}

outOfBounds :: Object -> Bool
outOfBounds obj = not (checkRectCollision obj background)


colEnemProj :: Game -> [Projectile] -> [Projectile]
colEnemProj _ [] = []
colEnemProj gameState@(GameState {player=ply@(Ship {ship_obj=ship_obj})}) (x@(Projectile {proj_obj=proj_obj}):xs) =
  if checkRectCollision ship_obj proj_obj || outOfBounds proj_obj then colEnemProj gameState xs else x : colEnemProj gameState xs

colPlyProj :: Game -> [Projectile] -> [Projectile]
colPlyProj _ [] = []
colPlyProj gameState@(GameState {enemies=enemies}) (proj:xs) = colPlyProjAux proj enemies ++ colPlyProj gameState xs
  where
    colPlyProjAux :: Projectile  -> [Ship] -> [Projectile]
    colPlyProjAux proj [] = [proj] 
    colPlyProjAux proj@(Projectile {proj_obj=proj_obj}) (x@(Ship{ship_obj=ship_obj}):xs) =
      if checkRectCollision proj_obj ship_obj || outOfBounds proj_obj then [] else colPlyProjAux proj xs



{-
collisionDespawn :: Game -> Game
collisionDespawn gameState@(GameState {npc_projectiles=npc_proj, ply_projectiles=ply_proj}) = gameState {npc_projectiles=desp_npc_proj, ply_projectiles=desp_ply_proj}
  where
    desp_ply_proj = colPlyProj gameState ply_proj
    desp_npc_proj = colEnemProj gameState npc_proj
-}

applyEffect :: Effect -> Ship -> Ship
applyEffect fx ship = 
  case fx of
    Damage x -> ship { ship_health = (shipHealth - x)}
    NoEffect -> ship -- do nothing
    where
      shipHealth = ship_health ship

getEffect :: Ship -> [Projectile] -> Effect
getEffect _ [] = NoEffect
getEffect ship@(Ship{ship_obj=ship_obj}) (x@(Projectile{effect=effect, proj_obj=proj_obj}):xs) =
  if checkRectCollision ship_obj proj_obj then effect else getEffect ship xs

updateEnemies :: Game -> [Ship] -> [Ship]
updateEnemies _ [] = []
updateEnemies gameState@(GameState {ply_projectiles=proj}) (ship:xs) =
  if ship_health ship <= 0  || playerCollideShip gameState ship then updateEnemies gameState xs
  else newShip : updateEnemies gameState xs
  where
    newShip = applyEffect (getEffect ship proj) ship



plyHandleDmg :: Game -> Ship -> Ship
plyHandleDmg gameState@(GameState {enemies=enemies ,npc_projectiles=npc_projectiles}) player@(Ship{ship_obj=ply_obj})
  | ship_health player <= 0 = enemyShipTest
  | foldl (||) False enemy_collisions = applyEffect (Damage 1) player
  | otherwise = applyEffect (getEffect player npc_projectiles) player
  where
    enemy_collisions = map (checkRectCollision ply_obj) enemy_objs
    enemy_objs = map ship_obj enemies


-- Collisiontests
o1 :: Object
o1 = Object { position = (2, 3),
              direction = (0, 0),
              speed = 0,
              boundingBox = (1, 1),
              graphic = color green $ rectangleSolid 50.0 50.0
            }
                                
o2 :: Object
o2 = Object { position = (0, 0),
              direction = (0, 0),
              speed = 0,
              boundingBox = (1, 1),
              graphic = color green $ rectangleSolid 50.0 50.0            }
     
o3 :: Object
o3 = Object { position = (0, 0),
              direction = (0, 0),
              speed = 300,
              boundingBox = (25, 25),
              graphic = enemySprite
            }

o4 :: Object
o4 = Object { position = (100, 100),
              direction = (0, 0),
              speed = 300,
              boundingBox = (25, 25),
              graphic = enemySprite
            }
o5 :: Object
o5 = Object { position = (200, 200),
              direction = (0, 0),
              speed = 300,
              boundingBox = (25, 49),
              graphic = enemySprite
            }

     
enemyShipTest :: Ship
enemyShipTest = Ship { ship_obj = o3,
                       ship_health = 3,
                       wep_cooldown = 2.0,
                       projectile = enemyDefaultProj,
                       last_fired_tick = 0,
                       isPlayer = False,
                       isFiring = True
                     }

enemyShipTest1 :: Ship
enemyShipTest1 = Ship { ship_obj = o4,
                       ship_health = 3,
                       wep_cooldown = 2.0,
                       projectile = enemyDefaultProj,
                       last_fired_tick = 0,
                       isPlayer = False,
                       isFiring = True
                     }

enemyShipTest2 :: Ship
enemyShipTest2 = Ship { ship_obj = o5,
                       ship_health = 3,
                       wep_cooldown = 2.0,
                       projectile = enemyDefaultProj,
                       last_fired_tick = 0,
                       isPlayer = False,
                       isFiring = True
                     }


