module Collision where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import DataTypes
import Globals
import Debug.Trace
import Projectile

--import Test.HUnit


{- checkRectCollision object1 object2
Checks is two objects overlap (collide).
PRE: 
RETURNS: True if object1 and object2 overlap in any way. Otherwise False.
EXAMPLES:
checkRectCollision (Object { position = (10, 0),
                             direction = (0, 0),
                             speed = 300,
                             bounds = (5, 5),
                             graphic = Blank
                           }) (Object { position = (0, 0),
                                        direction = (0, 0),
                                        speed = 300,
                                        bounds = (0, 0),
                                        graphic = Blank
                                      }) == True

-}

checkRectCollision :: Object -> Object -> Bool
checkRectCollision obj1@(Object {position=(x1, y1), bounds=(r1x, r1y)}) (Object {position=(x2, y2), bounds=(r2x, r2y)}) =
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
EXAMPLES: colEnemProj gameState (Projectile {projObj = smallObj,
                                             effect = Damage 1}) == []
(assuming the projectile in the list collides with the player in gameState)
-}

colEnemProj :: Game -> [Projectile] -> [Projectile]
--VARIANT: length of projs
colEnemProj _ [] = []
colEnemProj gameState@(GameState {player=ply@(Ship {shipObj=shipObj}),background=background}) (x@(Projectile {projObj=projObj}):xs) =
  if checkRectCollision shipObj projObj || (outOfBounds projObj background) then colEnemProj gameState xs else x : colEnemProj gameState xs

{-colPlyProj gameState projs
Checks the list of player projectiles to see if any of them need to be despawned. This happens if a projectile collides with an enemy or if the projectile is out of bounds.
PRE:
RETURNS: A list where the projectiles that should be despawned are removed.
EXAMPLES:colPlyProj gameState [(Projectile {projObj = smallObj,
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
      if (outOfBounds (projObj proj) background)
      then []
      else [proj]
    colPlyProjAux proj@(Projectile {projObj=projObj}) (x@(Ship{shipObj=shipObj}):xs) =
      if checkRectCollision projObj shipObj then [] else colPlyProjAux proj xs

{-applyEffect fx ship
Applies an effect to a Ship.
PRE:
RETURNS: A new Ship with the provided effect applied.
EXAMPLES:

-}
applyEffect :: Effect -> Ship -> Ship
applyEffect fx ship = 
  case fx of
    Damage x -> ship { shipHealth = (health - x)}
    NoEffect -> ship -- do nothing
    where
      health = shipHealth ship

{-getEffect ship projs
Checks if there is a Projectile colliding with a Ship and what effect it has.
PRE:
RETURNS: The Effect of the bullet that collides with the ship. Otherwise NoEffect.
EXAMPLES:
-}
getEffect :: Ship -> [Projectile] -> Effect
--VARIANT: length of projs
getEffect _ [] = NoEffect
getEffect ship@(Ship{shipObj=shipObj}) (x@(Projectile{effect=effect, projObj=projObj}):xs) =
  if checkRectCollision shipObj projObj then effect else getEffect ship xs

{-eneHandleDmg gameState ships
Checks if any Ships collide with a player bullet and reduces their hp accordingly. Also checks if the player collides with any of the Ships; said Ships are removed. If a Ship has 0 or less health it is removed.
PRE:
RETURNS: An updated list of Ships where the health values are adjusted and the ships with a health value of 0 or less are removed.
EXAMPLES:
-}
eneHandleDmg :: Game -> [Ship] -> [Ship]
--VARIANT: length of ships
eneHandleDmg _ [] = []
eneHandleDmg gameState@(GameState {player=player, plyProjectiles=proj}) (ship:xs) =
  if shipHealth ship <= 0  || playerCollideShip then eneHandleDmg gameState xs
  else newShip : eneHandleDmg gameState xs
  where
    newShip = applyEffect (getEffect ship proj) ship
    playerCollideShip = checkRectCollision (shipObj player) (shipObj ship)

{-plyHandleDmg gameState ship
Checks if a Ship collides with any of the enemy projectiles. If it does the hp of the ship is reduced accordingly. If the Ship collides with an enemy its health is reduced by 1. The Ship is replaced by an invisible Ship until the game is reset if health drops to 0.
PRE:
RETURNS: The Ship with an updated health value.
EXAMPLES:
-}
plyHandleDmg :: Game -> Ship -> Ship
plyHandleDmg gameState@(GameState {enemies=enemies ,enemyProjectiles=enemyProjectiles}) player@(Ship{shipObj=plyObj})
  | shipHealth player <= 0 = invisPlayer
  | foldl (||) False enemyCollisions = applyEffect (Damage 1) player
  | otherwise = applyEffect (getEffect player enemyProjectiles) player
  where
    enemyCollisions = map (checkRectCollision plyObj) enemyObjs
    enemyObjs = map shipObj enemies
    


--GAME OVER OBJECTS

--The object assigned to the player while the game is over.
gameOverObject :: Object
gameOverObject = Object { position = (1000, 1000),
                          direction = (0, 0),
                          speed = 300,
                          bounds = (0, 0),
                          graphic = Blank
                        }
--The player becomes an invisible ship while the game is over.                 
invisPlayer :: Ship
invisPlayer = Ship { shipObj = gameOverObject,
                     shipHealth = -1,
                     wepCooldown = 200.0,
                     projectile = harmlessProj,
                     lastFiredTick = 0,
                     isPlayer = False,
                     isFiring = False
                   }

