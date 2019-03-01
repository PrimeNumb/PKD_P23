-- This module should only contain functions that might be useful in several
-- modules but are problematic to categorize.
module Utilities where
import DataTypes

changeDir :: Object -> (Float, Float) -> Object
changeDir obj (x,y) = obj {direction = (x, y)}

-- Clamp an object within the bounds of a given bounding box

move :: Position -> Object -> Object
move (vx,vy) obj@(Object {position=(x,y)}) = obj { position = (x+vx,y+vy) }

setPos :: Position -> Object -> Object
setPos (x,y) obj@(Object {position=(xObj,yObj)}) = move (x-xObj,y-yObj) obj 

clampToBounds :: Bounds -> Object -> Object
clampToBounds (boundsWidth, boundsHeight) obj@(Object { position = (xPos, yPos), bounds = (objWidth, objHeight)}) =
  setPos (newXpos,newYpos) obj
  where
    -- There's a one-pixel border surrounding the game window.
    -- This border takes up part of the playable area on the x and y axis,
    -- hence the addition of (+1) in the declarations below to make sure part
    -- of the object isn't covered up by this border.
    newXpos
      | (xPos + objWidth) > boundsWidth = boundsWidth - objWidth
      | (xPos - (objWidth+1)) < -boundsWidth = -boundsWidth + objWidth+1
      | otherwise = xPos
    newYpos
      | (yPos + (objHeight+1)) > boundsHeight = boundsHeight - objHeight+1
      | (yPos - objHeight) < -boundsHeight = -boundsHeight + objHeight
      | otherwise = yPos

modDirection :: Direction -> Object -> Object
modDirection (dx, dy) obj@(Object {direction=(x,y)}) = obj { direction = (x+dx,y+dy)}


