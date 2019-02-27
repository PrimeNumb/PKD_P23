module Player where
import DataTypes
import Helpers
import Debug.Trace

-- Get the player position from a given game state
getPlayerPos :: Game -> Position
getPlayerPos gameState = position $ ship_obj $ player gameState

-- Get the player direction from a given game state
getPlayerDir :: Game -> Direction
getPlayerDir gameState = direction $ ship_obj $ player gameState

-- Returns a new game state where the player direction has been modified
modPlyDirection :: Game -> (Float, Float) -> Game
modPlyDirection gameState@(GameState {player=ply}) (x,y) = newGameState
  where
    plyObj = ship_obj (player gameState)
    (px,py) = direction plyObj
    (nx,ny) = (x+px,y+py)
    newPlyObj = plyObj {direction = (nx,ny)}
    newGameState = gameState {player = ply {ship_obj = newPlyObj}}

{- movePlayer object gameState deltaVector
   desc
   PRE: 
   RETURNS: 
   EXAMPLES: 
-}
--movePlayer :: Game -> (Float, Float) -> Game
--movePlayer gameState@(GameState {player=ply}) (dx, dy) = gameState { player = newPly}
--  where
--    (x, y) = position ply
--    (nx, ny) = (x+dx, y+dy)
--    newPly = ply { position = (nx, ny) } 

updatePlayer :: Float -> Game -> Ship
updatePlayer dt gameState@(GameState {ticker=currentTick,player=ply}) =
  newPlayer
  where
    -- Update the last fired tick
    canFire = (currentTick - (last_fired_tick ply)) > (wep_cooldown ply)
    updatedTick =
      case canFire of
        False -> last_fired_tick ply
        True  -> currentTick
    plyObj = ship_obj ply
    -- Movement
    (dx,dy) = direction plyObj
    plySpeed = speed plyObj
    deltaPos = (dx*plySpeed*dt,dy*plySpeed*dt) --rename this
    
    -- The new player 
    newPlayer =
      ply { ship_obj =
           (clampToBounds (playable_bounds gameState) $ move plyObj deltaPos), last_fired_tick = updatedTick }

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
