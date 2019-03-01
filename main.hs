module Main where
import Graphics.Gloss
import Graphics.Gloss.Juicy
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
                     graphic = color green $ rectangleSolid 50 50
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
           graphic = projObjDefault_gfx
         }
playerDefaultProj = Projectile playerDefaultProjObj (Damage 1)

defaultEncounter = Encounter
  { pop_interval = enemy_spawn_interval,
    last_pop = enemy_spawn_initial_delay,
    ship_stack = []
  }

defaultGameGFX = GameGFX
  {
    player_gfx = color green $ rectangleSolid 50 50,
    enemy_standard_gfx = color green $ rectangleSolid 50 50,
    player_proj_gfx = color green $ rectangleSolid 50 50,
    enemy_proj_gfx = color green $ rectangleSolid 50 50,
    heart_gfx = color green $ rectangleSolid 50 50,
    gameOver_gfx = color green $ rectangleSolid 50 50,
    background_gfx = color green $ rectangleSolid 50 50
  }


-- The initial game state
initGameState :: Game
initGameState = GameState {
  objects = [],
  game_gfx = defaultGameGFX,
  enemies = [],
  playable_bounds = (win_width/2, win_height/2),
  randomGen = mkStdGen 1234,
  encounter = defaultEncounter,
  player = playerShip,
  ply_projectiles = [],
  npc_projectiles = [],
  ticker = 0,
  background = defaultBackground,
  plyTemplate = playerShip,
  enmyTemplate = enemyShipDefaultTemplate,
  plyProjTemplate = playerDefaultProj,
  enmyProjTemplate = enemyDefaultProj,
  showHitbox  = False
  }


{- main
desc.
PRE: 
RETURNS: 
SIDE EFFECTS: 
EXAMPLES: 
-}
main :: IO ()
main = do
  --display window win_background (makeDrawable playerObj)
  seed <- randomIO :: IO Int
  -- Load sprites
  gameGFX <- loadGFX
  -- Process sprites
  let gen = mkStdGen seed
      readyGameState = refreshGFX $ initGameState {game_gfx=gameGFX,randomGen=newGen, encounter=defaultEncounter}
      (generatedShipStack, newGen) =
        generateEncounter gen 10 (enmyTemplate readyGameState)
      readyEncounter = defaultEncounter {ship_stack=generatedShipStack}
  play window win_background targetFramerate (readyGameState {randomGen=newGen, encounter=readyEncounter }) draw handleEvent update
  return ()

{-newGame gameState
Takes in the current GameState and resets it to default values allowing the player to start over.
PRE:
RETURNS: A new game state with reset values.
-}
newGame :: Game -> Game
newGame gameState@(GameState{randomGen=randomGen, enmyTemplate=enmyTemplate}) = gameState{encounter=initEncounter, player=(plyTemplate gameState), npc_projectiles=[], ply_projectiles=[], ticker=0, randomGen=newGen, objects=[], enemies=[]}
  where
  (generatedShipStack, newGen) = generateEncounter randomGen 10 enmyTemplate
  initEncounter = defaultEncounter {ship_stack=generatedShipStack}

refreshGFX :: Game -> Game
refreshGFX gameState@(GameState {game_gfx=gameGFX}) = newGameState
  where
    newPlyProj =
      setSprite (projectile $ player gameState) (player_proj_gfx gameGFX)
    newPlayerTemplate =
      (setSprite (player gameState) (player_gfx gameGFX)) { projectile = newPlyProj }
    newEnmyProj =
      setSprite (projectile $ enmyTemplate gameState) (enemy_proj_gfx gameGFX)
    newEnmyTemplate =
      (setSprite (enmyTemplate gameState) (enemy_standard_gfx gameGFX)) { projectile = newEnmyProj }
    newGameState = gameState
      {
        player = newPlayerTemplate,
        plyTemplate = newPlayerTemplate,
        enmyTemplate = newEnmyTemplate,
        plyProjTemplate = setSprite (plyProjTemplate gameState) (player_proj_gfx gameGFX),
        enmyProjTemplate = setSprite (enmyProjTemplate gameState) (enemy_proj_gfx gameGFX),
        background = setSprite (background gameState) (background_gfx gameGFX)
      }

loadGFX :: IO GameGFX
loadGFX = do
  imgBuffer <- loadJuicyPNG playerSpritePath
  let playerGFX = processSprite imgBuffer
  
  imgBuffer <- loadJuicyPNG enemySpritePath
  let enemyStandardGFX = processSprite imgBuffer

  imgBuffer <- loadJuicyPNG plyProjSpritePath
  let playerProjGFX = processSprite imgBuffer

  imgBuffer <- loadJuicyPNG enemyProjSpritePath
  let enemyProjGFX = processSprite imgBuffer

  imgBuffer <- loadJuicyPNG heartSpritePath
  let heartGFX = processSprite imgBuffer

  imgBuffer <- loadJuicyPNG gameOverSpritePath
  let gameOverGFX = processSprite imgBuffer
  
  imgBuffer <- loadJuicyPNG backgroundPath
  let backgroundGFX = processSprite imgBuffer
  let gameGFX =
        defaultGameGFX
        {
          player_gfx = playerGFX,
          enemy_standard_gfx = enemyStandardGFX,
          player_proj_gfx = playerProjGFX,
          enemy_proj_gfx = enemyProjGFX,
          heart_gfx = heartGFX,
          gameOver_gfx = gameOverGFX,
          background_gfx = backgroundGFX
        }
  return gameGFX

processSprite :: Maybe Picture -> Picture
processSprite Nothing = color green $ rectangleSolid 50 50
processSprite (Just pic) = pic

draw :: Game -> Picture
draw gameState@(GameState {objects=objs, game_gfx=gameGFX, player=playerShip, ply_projectiles=plyProjs, npc_projectiles=enemyProjs, enemies=enemies, showHitbox=showHitbox,background=background}) = newFrame
  where
    -- Everything that needs to be drawn goes here
    heartPics = map makeDrawable (updateHealthDisplay playerShip (heart_gfx gameGFX) (gameOver_gfx gameGFX))
    backgroundPic = makeDrawable background
    drawObjs =
      (map proj_obj enemyProjs) ++ (map proj_obj plyProjs) ++ (map ship_obj enemies) ++ (ship_obj playerShip):objs
    objPics = if showHitbox
      then (map drawWithBounds drawObjs)
      else (map makeDrawable drawObjs)
    -- The final picture frame
    newFrame = pictures $ (backgroundPic:heartPics) ++ objPics

{- update
   Updates a given game state one iteration.
   PRE:
   RETURNS:
   EXAMPLES:
-}

update :: Float -> Game -> Game
update dt gameState@(GameState {ticker=currentTick,ply_projectiles=projList, enemies=enemies,npc_projectiles=enemyProjList,encounter=encounter, player=player}) = newGameState 
  where
    -- Everything that should be updated each iteration goes here
    -- Player related
    newPlayer = (updatePlayer dt gameState{player=(plyHandleDmg gameState player)})
    updatePlyProjList =
      map (updateProjectile dt) (colPlyProj gameState projList)
    newPlyProjList = case (ship_fire (1,0) newTicker newPlayer) of
       Just x -> x:updatePlyProjList
       Nothing -> updatePlyProjList
       
    -- Enemy related
    (newEncounter, spawnedEnemies) =
      updateEncounter encounter currentTick enemies
    newEnemies = updateEnemies spawnedEnemies dt gameState
    updatedEnemyProjList =
      map (updateProjectile dt) (colEnemProj gameState enemyProjList)
    newEnemyProjList = (processEnemyFire gameState) ++ updatedEnemyProjList
    
    -- Game related
    newTicker = currentTick+dt
    
    -- The final updated gamestate
    newGameState = (gameState {player=newPlayer, ticker=newTicker, ply_projectiles=newPlyProjList, enemies=newEnemies, npc_projectiles=newEnemyProjList, encounter=newEncounter})

updateEncounter :: Encounter -> Float -> [Ship] -> (Encounter,[Ship])
updateEncounter encounter currentTick enemyContainer
  | shouldPopEncounter currentTick encounter = (newEncounter, newEnemyContainer)
  | otherwise = (encounter, enemyContainer)
  where
    updatedEncounter = encounter {last_pop=currentTick}
    (newStack, newEnemyContainer) =
      pop (ship_stack updatedEncounter) enemyContainer
    newEncounter = updatedEncounter {ship_stack=newStack}
{-updateHealthDisplay ship heartGFX gameOverGFX
Updates the health display of the Ship so it correlates with current health. Also displays a game over graphic when the player is dead.
PRE:
RETURNS: A list of the objects that are to be drawn. Either hearts corresponding to Ship health or a game over graphic.
Examples:
-}
updateHealthDisplay :: Ship -> Picture -> Picture -> [Object]
--VARIANT: ship_health ship
updateHealthDisplay player@(Ship{ship_health=ship_health}) heartGFX gameOverGFX
  |ship_health == -1 = [Object { position = (0, 0),
                                direction = (0, 0),
                                speed = 0,
                                boundingBox = (0, 0),
                                graphic = gameOverGFX
                               }]
  |ship_health <= 0 = []
  |otherwise = (Object { position = (xpos, 250),
                         direction = (0, 0),
                         speed = 0,
                         boundingBox = (0, 0),
                         graphic = heartGFX
                       }) : updateHealthDisplay newShip heartGFX gameOverGFX
  where
    newShip = player {ship_health=newHp}
    newHp = ship_health - 1
    xpos = fromIntegral (-500 + (40 * ship_health))
  
updateEnemies :: [Ship] -> Float -> Game -> [Ship]
updateEnemies enemies dt gameState = map (updateEnemy dt gameState) (eneHandleDmg gameState enemies)

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
    (SpecialKey KeySpace) -> gameState { player = (player gameState) {isFiring=True}, ply_projectiles = newPlyProjList }
      where
        currentTick = ticker gameState
        plyProjList = ply_projectiles gameState
        newPlyProjList =
          case (ship_fire (1,0) currentTick (player gameState)) of
            Just x  -> x:plyProjList
            Nothing -> plyProjList
    (SpecialKey KeyF1)    -> gameState { showHitbox = (not $ showHitbox gameState)}
    (SpecialKey KeyF2)            -> newGame gameState
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

  

