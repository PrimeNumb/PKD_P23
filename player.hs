module Player where
import DataTypes


-- Get the player position from a given game state
getPlayerPos :: Game -> Position
getPlayerPos gameState = position $ player gameState

-- Get the player direction from a given game state
getPlayerDir :: Game -> Direction
getPlayerDir gameState = direction $ player gameState

-- Returns a new game state where the player direction has been modified
modPlyDirection :: Game -> (Float, Float) -> Game
modPlyDirection gameState (x,y) = newGameState
  where
    (px,py) = direction (player gameState)
    (nx,ny) = (x+px,y+py)
    newGameState = gameState {player = (player gameState) { direction = (nx,ny)}}

{- movePlayer object gameState deltaVector
   desc
   PRE: 
   RETURNS: 
   EXAMPLES: 
-}
movePlayer :: Game -> (Float, Float) -> Game
movePlayer gameState@(GameState {player=ply}) (dx, dy) = gameState { player = newPly}
  where
    (x, y) = position ply
    (nx, ny) = (x+dx, y+dy)
    newPly = ply { position = (nx, ny) }

player_fire gameState = undefined

updatePlayer dt gameState@(GameState {player=ply}) = movePlayer gameState v
  where
    (dx,dy) = direction ply
    plySpeed = speed ply
    v = (dx*plySpeed*dt,dy*plySpeed*dt)

