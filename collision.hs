module Collision where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import DataTypes
import Globals
import Debug.Trace
import Projectile
import Test.HUnit


{- checkRectCollision object1 object2
Checks is two objects overlap (collide).
PRE: 
RETURNS: True if the objects collide, otherwise False.
EXAMPLES:
checkRectCollision (Object { position = (10, 0),
                             direction = (0, 0),
                             speed = 300,
                             boundingBox = (5, 5),
                             graphic = Blank
                           }) (Object { position = (0, 0),
                                        direction = (0, 0),
                                        speed = 300,
                                        boundingBox = (0, 0),
                                        graphic = Blank
                                      }) == True

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


{-outOfBounds object object2
Checks if an object is in not in contact with another object. We use this with the border (background) to decide if an object is out of bounds.
PRE:
RETURNS: True if the entire hitbox of the object is outside the bounding box of the other object, False otherwise.
EXAMPLES: outOfBounds (Object { position = (700, 700),
                               direction = (0, 0),
                               speed = 300,
                               boundingBox = (5, 5),
                               graphic = Blank
                           }) background == True
-}

outOfBounds :: Object -> Object -> Bool
outOfBounds obj obj2 = not (checkRectCollision obj obj2)

{-colEnemProj gameState projs
Checks the list of enemy projectiles to see if any of them need to be despawned. This happens if a projectile collides with the player or if the projectile is out of bounds.
PRE:
RETURNS: A list where the projectiles that should be despawned are removed.
EXAMPLES: colEnemProj gameState (Projectile {proj_obj = smallObj,
                                             effect = Damage 1}) == []
(assuming the projectile in the list collides with the player in gameState)
-}

colEnemProj :: Game -> [Projectile] -> [Projectile]
--VARIANT: length of projs
colEnemProj _ [] = []
colEnemProj gameState@(GameState {player=ply@(Ship {ship_obj=ship_obj}),background=background}) (x@(Projectile {proj_obj=proj_obj}):xs) =
  if checkRectCollision ship_obj proj_obj || (outOfBounds proj_obj background) then colEnemProj gameState xs else x : colEnemProj gameState xs

{-colPlyProj gameState projs
Checks the list of player projectiles to see if any of them need to be despawned. This happens if a projectile collides with an enemy or if the projectile is out of bounds.
PRE:
RETURNS: A list where the projectiles that should be despawned are removed.
EXAMPLES:colPlyProj gameState [(Projectile {proj_obj = smallObj,
                                            effect = Damage 1})] == [] 
(assuming the projectile in the list collides with an enemy in gameState)
-}
colPlyProj :: Game -> [Projectile] -> [Projectile]
--VARIANT: length of projs
colPlyProj _ [] = []
colPlyProj gameState@(GameState {enemies=enemies,background=background}) (proj:xs) = colPlyProjAux proj enemies ++ colPlyProj gameState xs
  where
    {-colPlyProjAux proj ships
      Checks if a player projectile is colliding with an enemy or is out of bounds. 
      RETURNS: The projectile if it doesn't collide and isn't out of bounds, otherwise [].
      EXAMPLES:
     -}
    colPlyProjAux :: Projectile  -> [Ship] -> [Projectile]
    --VARIANT: length of ships
    colPlyProjAux proj [] =
      if (outOfBounds (proj_obj proj) background)
      then []
      else [proj]
    colPlyProjAux proj@(Projectile {proj_obj=proj_obj}) (x@(Ship{ship_obj=ship_obj}):xs) =
      if checkRectCollision proj_obj ship_obj then [] else colPlyProjAux proj xs

{-applyEffect fx ship
Applies an effect to a Ship.
PRE:
RETURNS: A new Ship with the provided effect applied.
EXAMPLES:

-}
applyEffect :: Effect -> Ship -> Ship
applyEffect fx ship = 
  case fx of
    Damage x -> ship { ship_health = (shipHealth - x)}
    NoEffect -> ship -- do nothing
    where
      shipHealth = ship_health ship

{-getEffect ship projs
Checks if there is a Projectile colliding with a Ship and what effect it has.
PRE:
RETURNS: The Effect of the bullet that collides with the ship. Otherwise NoEffect.
EXAMPLES:
-}
getEffect :: Ship -> [Projectile] -> Effect
getEffect _ [] = NoEffect
getEffect ship@(Ship{ship_obj=ship_obj}) (x@(Projectile{effect=effect, proj_obj=proj_obj}):xs) =
  if checkRectCollision ship_obj proj_obj then effect else getEffect ship xs

{-eneHandleDmg gameState ships
Checks if any Ships collide with a player bullet and reduces their hp accordingly. Also checks if the player collides with any of the Ships; said Ships are removed. If a Ship has 0 or less health it is removed.
PRE:
RETURNS: An updated list of Ships where the health values are adjusted and the ships with a health value of 0 or less are removed.
EXAMPLES:
-}
eneHandleDmg :: Game -> [Ship] -> [Ship]
eneHandleDmg _ [] = []
eneHandleDmg gameState@(GameState {player=player, ply_projectiles=proj}) (ship:xs) =
  if ship_health ship <= 0  || playerCollideShip then eneHandleDmg gameState xs
  else newShip : eneHandleDmg gameState xs
  where
    newShip = applyEffect (getEffect ship proj) ship
    playerCollideShip = checkRectCollision (ship_obj player) (ship_obj ship)

{-plyHandleDmg gameState ship
Checks if a Ship collides with any of the enemy projectiles. If it does the hp of the ship is reduced accordingly. If the Ship collides with an enemy its health is reduced by 1. The Ship is replaced by an invisible Ship until the game is reset if health drops to 0.
PRE:
RETURNS: The Ship with an updated health value.
EXAMPLES:
-}
plyHandleDmg :: Game -> Ship -> Ship
plyHandleDmg gameState@(GameState {enemies=enemies ,npc_projectiles=npc_projectiles}) player@(Ship{ship_obj=ply_obj})
  | ship_health player <= 0 = invisPlayer
  | foldl (||) False enemy_collisions = applyEffect (Damage 1) player
  | otherwise = applyEffect (getEffect player npc_projectiles) player
  where
    enemy_collisions = map (checkRectCollision ply_obj) enemy_objs
    enemy_objs = map ship_obj enemies
    


--GAME OVER OBJECTS
gameOverObject :: Object
gameOverObject = Object { position = (1000, 1000),
                          direction = (0, 0),
                          speed = 300,
                          boundingBox = (0, 0),
                          graphic = Blank
                        }
invisPlayer :: Ship
invisPlayer = Ship { ship_obj = gameOverObject,
                     ship_health = -1,
                     wep_cooldown = 200.0,
                     projectile = harmlessProj,
                     last_fired_tick = 0,
                     isPlayer = False,
                     isFiring = False
                   }

--Testcases and test Objects


smallObj = Object { position = (15, 0),
                    direction = (0, 0),
                    speed = 300,
                    boundingBox = (5, 5),
                    graphic = Blank
                  }

bigObj = Object { position = (0, 0),
                  direction = (0, 0),
                  speed = 300,
                  boundingBox = (10, 10),
                  graphic = Blank
                }
       
objPoint = Object { position = (0, 0),
                    direction = (0, 0),
                    speed = 300,
                    boundingBox = (0, 0),
                    graphic = Blank
                  }


-- Collisiontests

test1 = TestCase $ assertEqual "bordering bounding boxes" False (checkRectCollision smallObj bigObj)

test2 = TestCase $ assertEqual "one object is a point (no area of the bounding box)" True (checkRectCollision bigObj objPoint)

runCollisionTests = runTestTT $ TestList [test1, test2]

