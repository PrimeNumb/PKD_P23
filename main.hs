module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import Debug.Trace
import Enemies
import Player
import Projectile
import DataTypes
import Globals
import Rendering
import Helpers
--import Collision
--import Control.Lens -- PLAN B: SOLVES NESTED RECORD FIELD HELL

-- The game window
window :: Display
window = InWindow win_title win_size win_offset

-- CHANGE THIS
playerObj :: Object
playerObj = Object { position = (0, 0),
                     direction = (0, 0),
                     speed = 300,
                     boundingBox = (25, -25),
                     graphic = color green $ rectangleSolid 50.0 50.0
                   }

-- Projectile templates
projObjDefault_spd :: Float
projObjDefault_spd = 400

projObjDefault_bbox :: BoundingBox
projObjDefault_bbox = (0,0)

projObjDefault_gfx :: Picture
projObjDefault_gfx = color red $ circleSolid 5

-- The initial game state
initGameState :: Game
initGameState = GameState {
  objects = [],
  player = playerObj,
  projectiles = [],
  ticker = 0,
  playerIsFiring = False,
  enemy = enemyObj1
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
draw gameState@(GameState {objects=objs, player=playerObj, projectiles=projs, enemy = enemyObj1}) = pictures $ player:enemy:projectiles ++ (map makeDrawable objs)
  where
    player = makeDrawable playerObj
    enemy = makeDrawable enemyObj1
    projectiles = map makeDrawable $ map proj_obj projs

{- update
   Updates a given game state one iteration.
   PRE:
   RETURNS:
   EXAMPLES:
-}
update :: Float -> Game -> Game
update dt gameState@(GameState {ticker=ticker,projectiles=projList}) = newGameState 
  where
    -- Everything that should be updated each iteration goes here
    newProjList = map (updateProjectile dt) projList
    newPlayer = updatePlayer dt gameState
    newTicker = ticker+dt
    --The final updated gamestate
    newGameState = gameState {player=newPlayer, ticker=newTicker, projectiles=newProjList}

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
    --(SpecialKey KeySpace) -> gameState { playerIsFiring = True}
    (SpecialKey KeySpace) -> spawnProjectile (Object (getPlayerPos gameState) (1,0) projObjDefault_spd projObjDefault_bbox projObjDefault_gfx) NoEffect gameState
    _ -> gameState
handleEvent (EventKey key Up _ _) gameState =
  case key of
    (SpecialKey KeyUp)    -> modPlyDirection gameState (0,-1)
    (SpecialKey KeyDown)  -> modPlyDirection gameState (0,1)
    (SpecialKey KeyLeft)  -> modPlyDirection gameState (1,0)
    (SpecialKey KeyRight) -> modPlyDirection gameState (-1,0)
    --(SpecialKey KeySpace) -> gameState { playerIsFiring = False}
    _ -> gameState
-- Need to do something when the screen is resized
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

isInBounds :: Point -> BoundingBox -> Point -> Bool
isInBounds (x, y) (width, height) (px, py) = xIsInBounds && yIsInBounds
  where
    (tx, ty) = (px-(width/2),py+(height/2))
    (bx, by) = (px+(width/2),py-(height/2))
    xIsInBounds = (tx <= x) && (x >= bx)
    yIsInBounds = (ty >= y) && (y >= by)


{- checkRectCollision
Checks is two objects overlap (collide).
PRE: The object's bounding boxes have their top left corner listed as the first 2-tuple. 
RETURNS:
EXAMPLES: 
-}

{- boundingBoxPoints bbox
Constructs a list out of all points (corners) in a bounding box, givens its position.
PRE: 
RETURNS: A list containing all points in bbox, where the first element is the point in the upper left corner of bbox and each proceeding element are all the points on the path going clockwise around bbox.
EXAMPLES: 
-}
boundingBoxPoints :: BoundingBox -> Point -> [Point]
boundingBoxPoints (width, height) (x,y) = points
  where
    (tl_x, tl_y) = (x-(width/2), y+(height/2))
    (tr_x, tr_y) = (x+(width/2), tl_y)
    (bl_x, bl_y) = (tl_x, y-(height/2))
    (br_x, br_y) = (tr_x, bl_y)
    points = [(tl_x,tl_y),(tr_x,tr_y),(br_x,br_y),(bl_x,bl_y)]

-- Test cases and test related functions go here for now
testGameState = initGameState -- this will change to more advanced test gamestates in the future

perfTest_spawnProj1 gs = spawnProjectile (Object (getPlayerPos gs) (getPlayerDir gs) projObjDefault_spd projObjDefault_bbox projObjDefault_gfx) NoEffect gs

perfTest_spawnProj2 gs = spawnProjectile testProjObj NoEffect gs

testGraphic = translate (-25) 25 $ circle 30
testObject =
  Object { position = (-25,25),
           direction = (0, 0),
           speed = 0,
           boundingBox = (0,0),
           graphic = testGraphic
         }

