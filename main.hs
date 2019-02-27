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
import Encounter

-- The game window
window :: Display
window = InWindow win_title win_size win_offset

-- CHANGE THIS
playerObj :: Object
playerObj = Object { position = (0, 0),
                     direction = (0, 0),
                     speed = 300,
                     boundingBox = (25, 25),
                     graphic = playerSprite
                   }
playerShip :: Ship
playerShip = Ship { ship_obj = playerObj,
                    ship_health = 3,
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
           graphic = plyProjSprite
         }
playerDefaultProj = Projectile playerDefaultProjObj (Damage 1)

initEncounterStack =
  EncounterStack { pop_interval = enemy_spawn_interval,
                   last_pop = 5.0,
                   ship_stack = [enemyShipTemplate,enemyShipTemplate]
                 }

-- The initial game state
initGameState :: Game
initGameState = GameState {
  playable_bounds = (win_width/2, win_height/2),
  objects = [],
  enemies = [(setPos (200,-200) enemyShipTemplate)],
  encounterStack = initEncounterStack,
  player = playerShip,
  ply_projectiles = [],
  npc_projectiles = [],
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
  --display window win_background (makeDrawable playerObj)
  play window win_background targetFramerate initGameState draw handleEvent update


draw :: Game -> Picture
draw gameState@(GameState {objects=objs, player=playerShip, ply_projectiles=plyProjs, npc_projectiles=enemyProjs, enemies=enemies}) = newFrame
  where
    -- Everything that needs to be drawn goes here
    playerPic = drawWithBounds playerShip
    plyProjPics = map drawWithBounds plyProjs
    enemyProjPics = map drawWithBounds enemyProjs
    enemyPics = map drawWithBounds enemies
    heartPics = map makeDrawable (updateHealthDisplay playerShip)
    backgroundPic = [(makeDrawable background)]
    -- The final picture frame
    newFrame = pictures $ backgroundPic ++ heartPics ++ enemyProjPics ++ plyProjPics ++ enemyPics ++ playerPic:(map makeDrawable objs)

{- update
   Updates a given game state one iteration.
   PRE:
   RETURNS:
   EXAMPLES:
-}

update :: Float -> Game -> Game
update dt gameState@(GameState {ticker=currentTick,ply_projectiles=projList, enemies=enemies,npc_projectiles=enemyProjList,encounterStack=eStack}) = newGameState 
  where
    -- Everything that should be updated each iteration goes here
    -- Player related
    updatePlyProjList = map (updateProjectile dt) (colPlyProj gameState projList)
    newPlayer = plyHandleDmg gameState (updatePlayer dt gameState)    
    newPlyProjList = case (ship_fire (1,0) newTicker newPlayer) of
       Just x -> x:updatePlyProjList
       Nothing -> updatePlyProjList
       
    -- Enemy related
    (newEncounterStack, spawnedEnemies) = updateEncounterStack eStack currentTick enemies
    newEnemies = updateEnemies spawnedEnemies dt gameState
    updatedEnemyProjList = map (updateProjectile dt) (colEnemProj gameState enemyProjList)
    newEnemyProjList = (processEnemyFire gameState) ++ updatedEnemyProjList
    
    -- Game related
    newTicker = currentTick+dt
    
    --The final updated gamestate
    newGameState = (gameState {player=newPlayer, ticker=newTicker, ply_projectiles=newPlyProjList, enemies=newEnemies, npc_projectiles=newEnemyProjList, encounterStack=newEncounterStack})

updateHealthDisplay :: Ship -> [Object]
updateHealthDisplay player@(Ship{ship_health=ship_health}) =
  if ship_health <= 0 then []
  else Object { position = (xpos, -250),
                direction = (0, 0),
                speed = 0,
                boundingBox = (0, 0),
                graphic = png "./sprites/heart.png"
              } : updateHealthDisplay player{ship_health=newHp}
  where
    newHp = ship_health - 1
    xpos = fromIntegral (-500 + (40 * ship_health))
    
updateEncounterStack :: EncounterStack -> Float -> [Ship] -> (EncounterStack,[Ship])
updateEncounterStack stack@(EncounterStack {}) currentTick enemyContainer
  | shouldPopEncounterStack currentTick stack = popEncounterStack updatedStack enemyContainer
  | otherwise = (stack, enemyContainer)
  where
    updatedStack = stack {last_pop=currentTick}
  
updateEnemies :: [Ship] -> Float -> Game -> [Ship]
updateEnemies enemies dt gameState = eneHandleDmg gameState (map (updateEnemy dt gameState) enemies)

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
ship_fire :: Direction -> Float -> Ship -> Maybe Projectile
ship_fire dir currentTick ship
  | canFire && (isFiring ship) = Just newShipProj
  | otherwise = Nothing
  where
    canFire = (currentTick - (last_fired_tick ship)) > (wep_cooldown ship)
    -- Construct the projectile object
    shipPos = position $ ship_obj ship
    shipProj = (projectile ship)
    shipProjObj = (proj_obj shipProj) { direction = dir, position = shipPos }
    newShipProj = Projectile shipProjObj (effect shipProj)


processEnemyFire :: Game -> [Projectile]
processEnemyFire gameState@(GameState {enemies=enemies,ticker=t}) = newProjList
  where
    traceStr = show newProjList
    newProjList = processEnemyFireAux (map (ship_fire (-1,0) t) enemies) []

processEnemyFireAux :: [Maybe Projectile] -> [Projectile] -> [Projectile]
processEnemyFireAux [] acc = acc
processEnemyFireAux (Just x : xs) acc = processEnemyFireAux xs (x:acc)
processEnemyFireAux (Nothing : xs) acc = processEnemyFireAux xs acc
    
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

  

