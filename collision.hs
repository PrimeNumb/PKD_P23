module Collision where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import DataTypes
import Globals
import Debug.Trace
import Projectile

{- checkRectCollision object1 object2
   Checks if two objects overlap (collide).
   PRE: True
   RETURNS: True if the bounds object1 and object2 overlap in any way. Otherwise False.
   EXAMPLES: checkRectCollision (Object (10,0) (0,0) 300 (5,5) Blank) (Object (0,0) (0,0) 300 (10,10) Blank) == True
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


{-outOfBounds object1 object2
   Checks if an object is outside the bounds of another object.
   PRE: True
   RETURNS: True if the bounds of object1 and object2 overlap, otherwise False.
EXAMPLES: outOfBounds (Object (700, 700) (0,0) 300 (5,5) Blank) (Object (0,0) (0,0) 300 (5,5) Blank) == True
-}
outOfBounds :: Object -> Object -> Bool
outOfBounds obj obj2 = not (checkRectCollision obj obj2)

{-colEnemProj gameState projs
   Processes collision of enemy projectiles with the player in a given game state.
   PRE: True
   RETURNS: A list containing any elements of projs that did not collide with the player.
   If all elements in projs collided, return the empty list.
   EXAMPLES: colEnemProj gameState (Projectile {projObj = smallObj,
                                             effect = Damage 1}) == []
-}
colEnemProj :: Game -> [Projectile] -> [Projectile]
--VARIANT: length of projs
colEnemProj _ [] = []
colEnemProj gameState@(GameState {player=ply@(Ship {shipObj=shipObj}),background=background}) (x@(Projectile {projObj=projObj}):xs) =
  if checkRectCollision shipObj projObj || (outOfBounds projObj background) then colEnemProj gameState xs else x : colEnemProj gameState xs

{- colPlyProj gameState projs
   Processes collision of player projectiles with any active enemies in a given game state.
   PRE: True
   RETURNS: A list containing elements of projs that did not collide with any active enemies.
   If all elements in projs collided, return the empty list.
   EXAMPLES: colPlyProj gameState [(Projectile {projObj = smallObj,
                                            effect = Damage 1})] == [] 
-}
colPlyProj :: Game -> [Projectile] -> [Projectile]
--VARIANT: length of projs
colPlyProj _ [] = []
colPlyProj gameState@(GameState {enemies=enemies,background=background}) (proj:xs) = colPlyProjAux proj enemies ++ colPlyProj gameState xs
  where
    {-colPlyProjAux proj ships
      Checks if a player projectile is colliding with an enemy or is out of bounds. 
      RETURNS: The projectile if it doesn't collide and isn't out of bounds, otherwise [].
      EXAMPLES: colPlyProjAux playerDefaultProj [] == [playerDefaultProj]
     -}
    colPlyProjAux :: Projectile  -> [Ship] -> [Projectile]
    --VARIANT: length of ships
    colPlyProjAux proj [] =
      if (outOfBounds (projObj proj) background)
      then []
      else [proj]
    colPlyProjAux proj@(Projectile {projObj=projObj}) (x@(Ship{shipObj=shipObj}):xs) =
      if checkRectCollision projObj shipObj then [] else colPlyProjAux proj xs

{- applyEffect fx ship
   Process an effect on a ship.
   PRE: True
   RETURNS: A new ship based on the value of fx.
   if fx is NoEffect then simply return ship.
   EXAMPLES: applyEffect NoEffect playerDefaultShip == playerDefaultShip
-}
applyEffect :: Effect -> Ship -> Ship
applyEffect fx ship = 
  case fx of
    Damage x -> ship { shipHealth = (health - x)}
    NoEffect -> ship -- do nothing
    where
      health = shipHealth ship

{- getEffect ship projs
   Checks if there is a projectile colliding with a ship and what effect it has.
   PRE: True
   RETURNS: If an element from projs collides with ship, return the first colliding element's effect. Otherwise if there are no colliding projectiles, return NoEffect.
   EXAMPLES: getEffect playerDefaultShip [] == NoEffect
-}
getEffect :: Ship -> [Projectile] -> Effect
--VARIANT: length of projs
getEffect _ [] = NoEffect
getEffect ship@(Ship{shipObj=shipObj}) (x@(Projectile{effect=effect, projObj=projObj}):xs) =
  if checkRectCollision shipObj projObj then effect else getEffect ship xs

{- eneHandleDmg gameState ships
   Checks if any ships collide with a player projectile and reduces their hp accordingly. Also checks if the player collides with any of the Ships; said Ships are removed. If a Ship has 0 or less health it is removed.
   PRE: True
   RETURNS: An updated list of Ships where the health values are adjusted and the ships with a health value of 0 or less are removed.
   EXAMPLES: eneHandleDmg defaultGameState [] == []
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

{- plyHandleDmg gameState ship
   Checks if a ship collides with any of the enemy projectiles. If it does the hp of the ship is reduced accordingly. If the Ship collides with an enemy its health is reduced by 1. The Ship is replaced by an invisible Ship until the game is reset if health drops to 0.
   PRE: True
   RETURNS: A new ship based on ship with an updated health value.
   EXAMPLES: plyHandleDmg defaultGameState playerDefaultShip == playerDefaultShip
-}
plyHandleDmg :: Game -> Ship -> Ship
plyHandleDmg gameState@(GameState {enemies=enemies ,enemyProjectiles=enemyProjectiles}) player@(Ship{shipObj=plyObj})
  | shipHealth player <= 0 = invisPlayer
  | foldl (||) False enemyCollisions = applyEffect (Damage 1) player
  | otherwise = applyEffect (getEffect player enemyProjectiles) player
  where
    enemyCollisions = map (checkRectCollision plyObj) enemyObjs
    enemyObjs = map shipObj enemies
