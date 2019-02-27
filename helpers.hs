-- NOTE: This module should probably renamed/discarded in the future.
-- This module should only contain functions that might be useful in several
-- modules but are problematic to categorize.
module Helpers where
import DataTypes

changeDir :: Object -> (Float, Float) -> Object
changeDir obj (x,y) = obj {direction = (x, y)}

--moveObject :: Object -> (Float, Float) -> Object
--moveObject obj@(Object {position = (x,y)}) (dx, dy) = obj { position = (nx,ny)}
--  where
--    (nx,ny) = (x+dx,y+dy)

-- Clamp an object within the bounds of a given bounding box
clampToBounds :: BoundingBox -> Object -> Object
clampToBounds (bounds_width, bounds_height) obj@(Object { position = (pos_x, pos_y), boundingBox = (obj_width, obj_height)}) =
  setPos (newPos_x,newPos_y) obj
  where
    -- There's a one-pixel border surrounding the game window.
    -- This border takes up part of the playable area on the x and y axis,
    -- hence the addition of (+1) in the declarations below to make sure part
    -- of the object isn't covered up by this border.
    newPos_x
      | (pos_x + obj_width) > bounds_width = bounds_width - obj_width
      | (pos_x - (obj_width+1)) < -bounds_width = -bounds_width + obj_width+1
      | otherwise = pos_x
    newPos_y
      | (pos_y + (obj_height+1)) > bounds_height = bounds_height - obj_height+1
      | (pos_y - obj_height) < -bounds_height = -bounds_height + obj_height
      | otherwise = pos_y



