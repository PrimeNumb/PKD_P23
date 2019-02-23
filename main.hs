module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import Debug.Trace
--import Enemies
import Projectile
import DataTypes
import Globals
import Rendering
import Helpers
--import Control.Lens -- PLAN B: SOLVES NESTED RECORD FIELD HELL

-- The game window
window :: Display
window = InWindow win_title win_size win_offset

-- CHANGE THIS
playerObj :: Object
playerObj = Object { position = (0, 0),
                     direction = (0, 0),
                     speed = 300,
                     boundingBox = ((0, 0), (0, 0)),
                     graphic = color green $ rectangleSolid 50.0 50.0
                   }

type Position = (Float, Float)
type Direction = (Float, Float)


-- Projectile templates
projObjDefault_spd :: Float
projObjDefault_spd = 400

projObjDefault_bbox :: BoundingBox
projObjDefault_bbox = ((0,0),(0,0))

projObjDefault_gfx :: Picture
projObjDefault_gfx = color red $ circleSolid 5

-- The initial game state
initGameState :: Game
initGameState = GameState {
  objects = [],
  player = playerObj,
  projectiles = []
 -- enemy = enermyObj1
 -- pressedKeys = []
  }

{- main
desc.
PRE: 
RETURNS: 
SIDE EFFECTS: 
EXAMPLES: 
-}
main :: IO()
main = do
--  let sampleCircle1 = translate 50 50 $ (circle 69)
--      sampleCircle2 = (circle 69)
--      toDraw = pictures [sampleCircle1, sampleCircle2]
  --display window win_background $ testProjGraphic
  play window win_background targetFramerate initGameState draw handleEvent update

{- draw gameState
   Constructs a drawable picture out of a given game state.
   PRE: 
   RETURNS:
   EXAMPLES: 
-}
draw :: Game -> Picture
draw gameState@(GameState {objects=objs, player=playerObj, projectiles=projs}) = pictures $ player:projectiles ++ (map makeDrawable objs)
  where
    player = makeDrawable playerObj
    projectiles = map makeDrawable $ map proj_obj projs

{- update
   Updates a given game state one iteration.
   PRE:
   RETURNS:
   EXAMPLES:
-}
update :: Float -> Game -> Game
update dt gameState = updatePlayer dt $ gameState { projectiles = updateProjectiles}
  where
    projList = projectiles gameState
    updateProjectiles = map (updateProjectile dt) projList
    

updatePlayer dt gameState@(GameState {player=ply}) = movePlayer gameState v
  where
    (dx,dy) = direction ply
    plySpeed = speed ply
    v = (dx*plySpeed*dt,dy*plySpeed*dt)

{- handleEvent gameState
Calls a specific
   PRE:
   RETURNS:
   EXAMPLES:
-}
handleEvent :: Event -> Game -> Game
handleEvent (EventKey key Down mod _) gameState =
  case key of
    (SpecialKey KeyUp)    -> modPlyDirection gameState (0,1)
    (SpecialKey KeyDown)  -> modPlyDirection gameState (0,-1)
    (SpecialKey KeyLeft)  -> modPlyDirection gameState (-1,0)
    (SpecialKey KeyRight) -> modPlyDirection gameState (1,0)
    (SpecialKey KeySpace) -> spawnProjectile (Object (getPlayerPos gameState) (1,0) projObjDefault_spd projObjDefault_bbox projObjDefault_gfx) NoEffect gameState
    _ -> gameState
handleEvent (EventKey key Up _ _) gameState =
  case key of
    (SpecialKey KeyUp)    -> modPlyDirection gameState (0,-1)
    (SpecialKey KeyDown)  -> modPlyDirection gameState (0,1)
    (SpecialKey KeyLeft)  -> modPlyDirection gameState (1,0)
    (SpecialKey KeyRight) -> modPlyDirection gameState (-1,0)
    (SpecialKey KeySpace) -> gameState
    _ -> gameState
-- Need to handle 
handleEvent (EventResize (x, y)) gameState = gameState
handleEvent _ gameState = gameState

-- Check if an object is inside a bounding box
-- Returns a tuple where
-- 1st element is TRUE if the obj is partially inside the bounding box
-- 2nd element is TRUE if the obj is completely inside the bounding box
--checkCollision :: Object -> BoundingBox -> (Bool, Bool)
--checkCollision obj bbox = undefined
--  where
--    obj_bbox = boundingBox obj
--    partialCollision = 


isInBounds :: Point -> BoundingBox -> Bool
isInBounds (x, y) ((tx, ty), (bx, by)) = xIsInBounds && yIsInBounds
  where
    xIsInBounds = (tx <= x) && (x >= bx)
    yIsInBounds = (ty >= y) && (y >= by)

{- boundingBoxPoints bbox
Constructs a list out of all points (corners) in a bounding box.
PRE: 
RETURNS: A list containing all points in bbox, where the first element is the point in the upper left corner of bbox and each proceeding element are all the points on the path going clockwise around bbox.
EXAMPLES: 
-}
boundingBoxPoints :: BoundingBox -> [Point]
boundingBoxPoints ((tl_x, tl_y), (br_x, br_y)) = points
  where
    width = abs tl_x-br_x
    height = abs tl_y-br_y
    tr_x = tl_x + width
    tr_y = tl_y
    bl_x = br_x
    bl_y = tl_y - height
    points = [(tl_x,tl_y),(tr_x,tr_y),(br_x,br_y),(bl_x,bl_y)]

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


-- Test cases and test related functions go here for now
testGameState = initGameState -- this will change to more advanced test gamestates in the future

perfTest_spawnProj1 gs = spawnProjectile (Object (getPlayerPos gs) (getPlayerDir gs) projObjDefault_spd projObjDefault_bbox projObjDefault_gfx) NoEffect gs

perfTest_spawnProj2 gs = spawnProjectile testProjObj NoEffect gs

testGraphic = translate (-25) 25 $ circle 30
testObject =
  Object { position = (-25,25),
           direction = (0, 0),
           speed = 0,
           boundingBox = ((-55, 55), (5, -5)),
           graphic = testGraphic
         }
