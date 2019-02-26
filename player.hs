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
    newPlayer = ply {ship_obj = (moveObject plyObj deltaPos), last_fired_tick = updatedTick}

