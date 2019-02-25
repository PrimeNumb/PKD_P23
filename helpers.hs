-- NOTE: This module should probably renamed/discarded in the future.
-- This module should only contain functions that might be useful in several
-- modules but are problematic to categorize.
module Helpers where
import DataTypes


moveObject :: Object -> (Float, Float) -> Object
moveObject obj@(Object {position = (x,y)}) (dx, dy) = obj { position = (nx,ny)}
  where
    (nx,ny) = (x+dx,y+dy)





