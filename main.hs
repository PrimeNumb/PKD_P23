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
import Collision
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
                     boundingBox = (25, 25),
                     graphic = color green $ rectangleSolid 50.0 50.0
                   }
playerShip :: Ship
playerShip = Ship { ship_obj = playerObj,
                    ship_health = 100,
                    wep_cooldown = 0.25,
                    projectile = playerDefaultProj,
                    last_fired_tick = 0,
                    isFiring = False,
                    isPlayer = True
                  }

playerDefaultProjObj =
  Object { position = (0,0),
           direction = (1,0),
           speed = projObjDefault_spd,
           boundingBox = projObjDefault_bbox,
           graphic = projObjDefault_gfx
         }
playerDefaultProj = Projectile playerDefaultProjObj (Damage 1)
                  
-- Projectile templates
projObjDefault_spd :: Float
projObjDefault_spd = 400

projObjDefault_bbox :: BoundingBox
projObjDefault_bbox = (2.5,2.5)

projObjDefault_gfx :: Picture
projObjDefault_gfx = color red $ circleSolid 5

-- The initial game state
initGameState :: Game
initGameState = GameState {
  objects = [],
  enemies = [enemyShipTemplate, enemyShipTest],
  player = playerShip,
  ply_projectiles = [],
  npc_projectiles = [],
  enemy = enemyShipTemplate,
  ticker = 0,
  playerIsFiring = False
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
draw gameState@(GameState {objects=objs, player=playerShip, ply_projectiles=plyProjs, enemy = enemyObj1, enemies=enemies}) = newFrame
  where
    -- Everything that needs to be drawn goes here
    playerObj = makeDrawable (ship_obj playerShip)
    enemy = makeDrawable (ship_obj enemyObj1)
    plyProjectiles = map makeDrawable $ map proj_obj plyProjs
    enemyPics = map makeDrawable (map ship_obj enemies) 
    -- The final picture frame
    newFrame = pictures $ enemyPics ++ plyProjectiles ++ enemy:playerObj:(map makeDrawable objs)

{- update
   Updates a given game state one iteration.
   PRE:
   RETURNS:
   EXAMPLES:
-}
update :: Float -> Game -> Game
update dt gameState@(GameState {ticker=ticker,ply_projectiles=projList,enemy=enemy, enemies=enemies}) = newGameState 
  where
    -- Everything that should be updated each iteration goes here
    newPlyProjList = map (updateProjectile dt) (colPlyProj gameState projList)
    newPlayer = updatePlayer dt gameState
    newTicker = ticker+dt
    newEnemy = updateEnemy dt gameState
    newEnemies = updateEnemies gameState enemies
    --traceStr = "Tick: " ++ show ticker ++ (show $ direction $ ship_obj newEnemy)
    --The final updated gamestate
    newGameState = ship_fire newPlayer (1,0) (gameState {player=newPlayer, ticker=newTicker, ply_projectiles=newPlyProjList, enemy=newEnemy})

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
    (SpecialKey KeySpace) -> gameState { player = (player gameState) {isFiring=True}}
    _ -> gameState
handleEvent (EventKey key Up _ _) gameState =
  case key of
    (SpecialKey KeyUp)    -> modPlyDirection gameState (0,-1)
    (SpecialKey KeyDown)  -> modPlyDirection gameState (0,1)
    (SpecialKey KeyLeft)  -> modPlyDirection gameState (1,0)
    (SpecialKey KeyRight) -> modPlyDirection gameState (-1,0)
    (SpecialKey KeySpace) -> gameState { player = (player gameState) {isFiring=False}}
    _ -> gameState
-- Need to do something when the screen is resized
handleEvent (EventResize (x, y)) gameState = gameState
handleEvent _ gameState = gameState

-- Fires a ship's projectile from its position, given a direction
ship_fire :: Ship -> Direction -> Game -> Game
ship_fire ship dir gameState@(GameState {ticker=currentTick})
  | canFire && (isFiring ship) = spawnProjectile newShipProj isPC gameState
  | otherwise = gameState
  where
    canFire = (currentTick - (last_fired_tick ship)) > (wep_cooldown ship)
    isPC = isPlayer ship
    -- Construct the projectile object
    shipPos = position $ ship_obj ship
    shipProj = (projectile ship)
    shipProjObj = (proj_obj shipProj) { direction = dir, position = shipPos }
    newShipProj = Projectile shipProjObj (effect shipProj)

--processShipWeapons :: Game -> Game
--processShipWeapons gameState@(GameState {player=ply,enemies=npcs,ply_projectiles=plyProjList,npc_projectles=npcProjList}) = 
--  where
--    newPlyProjList = plyProjList
--    newGameState = gameState

    
-- Check if an object is inside a bounding box
-- Returns a tuple where
-- 1st element is TRUE if the obj is partially inside the bounding box
-- 2nd element is TRUE if the obj is completely inside the bounding box
--checkCollision :: Object -> BoundingBox -> (Bool, Bool)
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

testGraphic = translate (-25) 25 $ circle 30
testObject =
  Object { position = (-25,25),
           direction = (0, 0),
           speed = 0,
           boundingBox = (0,0),
           graphic = testGraphic
         }

