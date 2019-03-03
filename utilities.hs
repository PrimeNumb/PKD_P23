module Utilities where
import DataTypes

{- changeDir object1 newDirection
   Changes the direction of an object.
   PRE: True
   RETURNS: An object based on object1 with a new direction based on newDirection.
   EXAMPLES: changeDir playerObj (0,0) == playerObj {direction = (0,0)}
-}
changeDir :: Object -> (Float, Float) -> Object
changeDir obj (x,y) = obj {direction = (x, y)}

{- move deltaPos obj
   Moves an object relative to its current position.
   PRE: True
   RETURNS: An object based on obj, where its position is based on its old position and deltaPos.
   EXAMPLES: move (10, 0) (Object (10,0) (0,0) 300.0 (10,10) Blank) ==
   (Object (20,0) (0,0) 300.0 (10,10) Blank)
-}
move :: Position -> Object -> Object
move (vx,vy) obj@(Object {position=(x,y)}) = obj { position = (x+vx,y+vy) }

{- setPos newPosition obj
   Sets the absolute position of an object.
   PRE: True
   RETURNS: A new object based on obj, where its position is newPosition
   EXAMPLES: setPos (10, 15) (Object (0,0) (0,0) 300.0 (10,10) Blank) ==
   (Object (10,15) (0,0) 300.0 (10,10) Blank)
-}
setPos :: Position -> Object -> Object
setPos (x,y) obj@(Object {position=(xObj,yObj)}) = move (x-xObj,y-yObj) obj 

{- clampToBounds obj1 obj2
   Clamp an object within the bounds of a given object.
   PRE: True
   RETURNS: An object based on obj2, where the boundaries of the object are inside the boundaries of obj1.
   EXAMPLES: clampToBounds (10,0) (Object (0,0) (0,0) 300.0 (10,10) Blank) ==
   (Object (0,-10) (0,0) 300.0 (10,10) Blank)
-}
clampToBounds :: Object -> Object -> Object
clampToBounds bObj@(Object {position = (bx,by),bounds=(boundsWidth,boundsHeight)}) obj@(Object { position = (xPos, yPos), bounds = (objWidth, objHeight)}) =
  setPos (newXpos,newYpos) obj
  where
    
    newXpos
      | (xPos + objWidth) > (boundsWidth+bx) = (boundsWidth+bx) - objWidth
      | (xPos - (objWidth)) < (-boundsWidth+bx) = (-boundsWidth+bx) + objWidth
      | otherwise = xPos
    newYpos
      | (yPos + (objHeight)) > (boundsHeight+by) = (boundsHeight+by) - objHeight
      | (yPos - objHeight) < (-boundsHeight+by) = (-boundsHeight+by) + objHeight
      | otherwise = yPos

{- modDirection deltaDir obj
   Changes an object's direction relative to its current direction.
   PRE: True
   RETURNS: An object based on obj, where its direction is based on the direction of obj and deltaDir.
   EXAMPLES: modDirection (-1,0) (Object (0,0) (1,0) 300.0 (10,10) Blank) ==
   (Object (0,0) (0,0) 300.0 (10,10) Blank)
-}
modDirection :: Direction -> Object -> Object
modDirection (dx, dy) obj@(Object {direction=(x,y)}) = obj { direction = (x+dx,y+dy)}

{- shipFire arguments
Fires a ship's projectile from its position, given a direction
PRE: True
RETURNS: Just projectile where the projectile appears at a ship with given direction or Nothing
EXAMPLES:
-}
shipFire :: Direction -> Float -> Ship -> Maybe Projectile
shipFire dir currentTick ship
  | canFire && (isFiring ship) = Just newShipProj
  | otherwise = Nothing
  where
    canFire = (currentTick - (lastFiredTick ship)) > (wepCooldown ship)
    -- Construct the projectile object
    shipPos = position $ shipObj ship
    shipProj = (projectile ship)
    shipProjObj = (projObj shipProj) { direction = dir, position = shipPos }
    newShipProj = Projectile shipProjObj (effect shipProj)
