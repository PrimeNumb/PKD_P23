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
import Utilities
import Collision
import Encounter

-- The game window
window :: Display
window = InWindow winTitle winSize winOffset


--The default encounters

defaultEncounter = Encounter
  { popInterval = enemySpawnInterval,
    lastPop = enemySpawnInitialDelay,
    shipStack = []
  }

--The default graphics before sprites
defaultGameGfx = GameGfx
  {
    playerGfx = color green $ rectangleSolid 50 50,
    enemyStandardGfx = color green $ rectangleSolid 50 50,
    playerProjGfx = color green $ rectangleSolid 50 50,
    enemyProjGfx = color green $ rectangleSolid 50 50,
    heartGfx = color green $ rectangleSolid 50 50,
    gameOverGfx = color green $ rectangleSolid 50 50,
    backgroundGfx = color green $ rectangleSolid 50 50
  }


-- The initial game state
initGameState :: Game
initGameState = GameState {
  objects = [],
  gameGfx = defaultGameGfx,
  enemies = [],
  playableBounds = (winWidth/2, winHeight/2),
  randomGen = mkStdGen 1234,
  encounter = defaultEncounter,
  player = playerShipDefault,
  plyProjectiles = [],
  enemyProjectiles = [],
  ticker = 0,
  background = defaultBackground,
  plyTemplate = playerShipDefault,
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
  --display window winBackground (makeDrawable playerObj)
  seed <- randomIO :: IO Int
  -- Load sprites
  gameGfx <- loadGfx
  -- Process sprites
  let gen = mkStdGen seed
      readyGameState = refreshGfx $ initGameState {gameGfx=gameGfx,randomGen=newGen, encounter=defaultEncounter}
      (generatedShipStack, newGen) =
        generateEncounter gen 10 (enmyTemplate readyGameState)
      readyEncounter = defaultEncounter {shipStack=generatedShipStack}
  play window winBackground targetFramerate (readyGameState {randomGen=newGen, encounter=readyEncounter }) draw handleEvent update
  return ()

{-newGame gameState
  Takes in the current GameState and resets it to default values allowing the player to start over.
  PRE: True
  RETURNS: A new game state with reset values.
-}
newGame :: Game -> Game
newGame gameState@(GameState{randomGen=randomGen, enmyTemplate=enmyTemplate}) = gameState{encounter=initEncounter, player=(plyTemplate gameState), enemyProjectiles=[], plyProjectiles=[], ticker=0, randomGen=newGen, objects=[], enemies=[]}
  where
  (generatedShipStack, newGen) = generateEncounter randomGen 10 enmyTemplate
  initEncounter = defaultEncounter {shipStack=generatedShipStack}

{-refreshGFX gameState
  Takes in a 
  PRE: True
  RETURNS: A new game state with reset values.
-}
refreshGfx :: Game -> Game
refreshGfx gameState@(GameState {gameGfx=gameGfx}) = newGameState
  where
    newPlyProj =
      (projectile $ player gameState) {projObj = setGraphic (projObj $ projectile $ player gameState) (playerProjGfx gameGfx)}
      --setGraphic (projObj $ projectile $ player gameState) (playerProjGfx gameGfx)
    newPlayerTemplate =
      (player gameState) {shipObj = setGraphic (shipObj $ player gameState) (playerGfx gameGfx), projectile = newPlyProj}
    newEnmyProj =
      (projectile $ enmyTemplate gameState) {projObj = setGraphic (projObj $ projectile $ enmyTemplate gameState) (enemyProjGfx gameGfx)}
    newEnmyTemplate =
      (enmyTemplate gameState) {shipObj = (setGraphic (shipObj $ enmyTemplate gameState) (enemyStandardGfx gameGfx)), projectile = newEnmyProj }
    newGameState = gameState
      {
        player = newPlayerTemplate,
        plyTemplate = newPlayerTemplate,
        enmyTemplate = newEnmyTemplate,
        plyProjTemplate = (plyProjTemplate gameState) {projObj = setGraphic (projObj $ plyProjTemplate gameState) (playerProjGfx gameGfx)},
        enmyProjTemplate = (enmyProjTemplate gameState) {projObj = setGraphic (projObj $ enmyProjTemplate gameState) (enemyProjGfx gameGfx)},
        background = setGraphic (background gameState) (backgroundGfx gameGfx)
      }
{- loadGFX
   Loads pictures from predestined filepaths into a GameGFX.
   PRE: True
   RETURNS: A GameGFX containing the pictures.
   SIDE EFFECTS: IO; loading images. The exception handling if a filepath is invalid is handled in loadJuicyPNG. It returns a Maybe Picture or Nothing. 
   EXAMPLES: 
-}
loadGfx :: IO GameGfx
loadGfx = do
  imgBuffer <- loadJuicyPNG playerSpritePath
  let playerGfx = processSprite imgBuffer
  
  imgBuffer <- loadJuicyPNG enemySpritePath
  let enemyStandardGfx = processSprite imgBuffer

  imgBuffer <- loadJuicyPNG plyProjSpritePath
  let playerProjGfx = processSprite imgBuffer

  imgBuffer <- loadJuicyPNG enemyProjSpritePath
  let enemyProjGfx = processSprite imgBuffer

  imgBuffer <- loadJuicyPNG heartSpritePath
  let heartGfx = processSprite imgBuffer

  imgBuffer <- loadJuicyPNG gameOverSpritePath
  let gameOverGfx = processSprite imgBuffer
  
  imgBuffer <- loadJuicyPNG backgroundPath
  let backgroundGfx = processSprite imgBuffer
  let gameGfx =
        defaultGameGfx
        {
          playerGfx = playerGfx,
          enemyStandardGfx = enemyStandardGfx,
          playerProjGfx = playerProjGfx,
          enemyProjGfx = enemyProjGfx,
          heartGfx = heartGfx,
          gameOverGfx = gameOverGfx,
          backgroundGfx = backgroundGfx
        }
  return gameGfx

{- processSprite pic
   Processes a Maybe Picture into either a Picture or a placehoder graphic.
   PRE: True
   RETURNS: The Picture if there is one. Otherwise a placeholder graphic.
   EXAMPLES: processSprite Nothing == Color (RGBA 0.0 1.0 0.0 1.0) (Polygon [(-25.0,-25.0),(-25.0,25.0),(25.0,25.0),(25.0,-25.0)])
-}
processSprite :: Maybe Picture -> Picture
processSprite Nothing = color green $ rectangleSolid 50 50
processSprite (Just pic) = pic

{- draw gameState
   Takes in all objects that need to be drawn from a game state and combines them into a picture.
   PRE: True
   RETURNS: A combined picture of all pictures that are to be drawn.
   EXAMPLES:
-}
draw :: Game -> Picture
draw gameState@(GameState {objects=objs, gameGfx=gameGfx, player=playerShip, plyProjectiles=plyProjs, enemyProjectiles=enemyProjs, enemies=enemies, showHitbox=showHitbox,background=background}) = newFrame
  where
    -- Everything that needs to be drawn goes here
    heartPics = map makeDrawable (updateHealthDisplay playerShip (heartGfx gameGfx) (gameOverGfx gameGfx))
    backgroundPic = makeDrawable background
    drawObjs =
      (map projObj enemyProjs) ++ (map projObj plyProjs) ++ (map shipObj enemies) ++ (shipObj playerShip):objs
    objPics = if showHitbox
      then (map makeDrawable drawObjs) ++ (map drawBounds drawObjs)
      else (map makeDrawable drawObjs)
    -- The final picture frame
    newFrame = pictures $ (backgroundPic:heartPics) ++ objPics

{- update dt gameState
   Updates a given game state one iteration.
   PRE: True
   RETURNS: An updated gameState.
   EXAMPLES:
-}
update :: Float -> Game -> Game
update dt gameState@(GameState {ticker=currentTick,plyProjectiles=projList, enemies=enemies,enemyProjectiles=enemyProjList,encounter=encounter, player=player}) = newGameState 
  where
    -- Everything that should be updated each iteration goes here
    -- Player related
    newPlayer = (updatePlayer dt gameState{player=(plyHandleDmg gameState player)})
    updatePlyProjList =
      map (updateProjectile dt) (colPlyProj gameState projList)
    newPlyProjList = case (shipFire (1,0) newTicker newPlayer) of
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
    newGameState = (gameState {player=newPlayer, ticker=newTicker, plyProjectiles=newPlyProjList, enemies=newEnemies, enemyProjectiles=newEnemyProjList, encounter=newEncounter})


{-updateEncounter encounter currentTick enemyContainer
  Checks if an enemy should spawn from an Encounter stack by looking at the current tick. If that is the case it is moved into a list if ships.
  PRE: True
  RETURNS: A 2-tuple of the updated encounter stack and the updated list of ships.
  Examples:
-}
updateEncounter :: Encounter -> Float -> [Ship] -> (Encounter,[Ship])
updateEncounter encounter currentTick enemyContainer
  | shouldPopEncounter currentTick encounter = (newEncounter, newEnemyContainer)
  | otherwise = (encounter, enemyContainer)
  where
    updatedEncounter = encounter {lastPop=currentTick}
    (newStack, newEnemyContainer) =
      pop (shipStack updatedEncounter) enemyContainer
    newEncounter = updatedEncounter {shipStack=newStack}

{-updateHealthDisplay ship heartGFX gameOverGFX
  Updates the health display of the Ship so it correlates with current health. Also displays a game over graphic when the player is dead.
  PRE:
  RETURNS: A list of the objects that are to be drawn. Either hearts corresponding to Ship health or a game over graphic.
  Examples:
-}

updateHealthDisplay :: Ship -> Picture -> Picture -> [Object]
--VARIANT: shipHealth ship
updateHealthDisplay player@(Ship{shipHealth=shipHealth}) heartGfx gameOverGfx
  |shipHealth == -1 = [Object { position = (0, 0),
                                direction = (0, 0),
                                speed = 0,
                                bounds = (0, 0),
                                graphic = gameOverGfx
                               }]
  |shipHealth <= 0 = []
  |otherwise = (Object { position = (xpos, 250),
                         direction = (0, 0),
                         speed = 0,
                         bounds = (0, 0),
                         graphic = heartGfx
                       }) : updateHealthDisplay newShip heartGfx gameOverGfx
  where
    newShip = player {shipHealth=newHp}
    newHp = shipHealth - 1
    xpos = fromIntegral (-500 + (40 * shipHealth))

{- updateEnemies enemies dt gameState
   Updates a list of enemies. The health, position and the time since they last    fired is updated. Enemies are also removed if their health is depleted.
   PRE: True
   RETURNS: A list of updated enemies in regards to the given gameState.
   EXAMPLES: 
-}  
updateEnemies :: [Ship] -> Float -> Game -> [Ship]
updateEnemies enemies dt gameState = map (updateEnemy dt gameState) (eneHandleDmg gameState enemies)

{- handleEvent event gameState
   Takes in an event and modifies a given gameState according to the               Event (input).
   PRE: True
   RETURNS: A gameState with changed values depending on the Event.
   EXAMPLES:
-}
handleEvent :: Event -> Game -> Game
handleEvent (EventKey key Down mod _) gameState@(GameState {player=player}) =
  case key of
    (SpecialKey KeyUp)    -> gameState { player = player {shipObj = modDirection (0,1) (shipObj player)}}
    (SpecialKey KeyDown)  -> gameState { player = player {shipObj = modDirection (0,-1) (shipObj player)}}
    (SpecialKey KeyLeft)  -> gameState { player = player {shipObj = modDirection (-1,0) (shipObj player)}}
    (SpecialKey KeyRight) -> gameState { player = player {shipObj = modDirection (1,0) (shipObj player)}}
    (SpecialKey KeySpace) -> gameState { player = player {isFiring=True}, plyProjectiles = newPlyProjList }
      where
        currentTick = ticker gameState
        plyProjList = plyProjectiles gameState
        newPlyProjList =
          case (shipFire (1,0) currentTick player) of
            Just x  -> x:plyProjList
            Nothing -> plyProjList
    (SpecialKey KeyF1)    -> gameState { showHitbox = (not $ showHitbox gameState)}
    (SpecialKey KeyF2)            -> newGame gameState
    _ -> gameState
handleEvent (EventKey key Up _ _) gameState@(GameState {player=player}) =
  case key of
    (SpecialKey KeyUp)    -> gameState { player = player {shipObj = modDirection (0,-1) (shipObj player)}} 
    (SpecialKey KeyDown)  -> gameState { player = player {shipObj = modDirection (0,1) (shipObj player)}} 
    (SpecialKey KeyLeft)  -> gameState { player = player {shipObj = modDirection (1,0) (shipObj player)}} 
    (SpecialKey KeyRight) -> gameState { player = player {shipObj = modDirection (-1,0) (shipObj player)}}
    (SpecialKey KeySpace) -> gameState { player = player {isFiring=False}}
    _ -> gameState
-- Need to do something when the screen is resized
handleEvent (EventResize (x, y)) gameState = gameState
handleEvent _ gameState = gameState

  {- shipFire arguments
     Fires a ship's projectile from its position, given a direction
     PRE: True
     RETURNS: Just projectile where the projectile appears at a ship with given direction or Nothing
     EXAMPLES:
  -}
shipFire :: Direction -> Float -> Ship -> Maybe Projectile
shipFire dir currentTick ship
  | canFire && (isFiring ship) = Just newShipProj
  | otherwise = Nothing
  where
    canFire = (currentTick - (lastFiredTick ship)) > (wepCooldown ship)
    -- Construct the projectile object
    shipPos = position $ shipObj ship
    shipProj = (projectile ship)
    shipProjObj = (projObj shipProj) { direction = dir, position = shipPos }
    newShipProj = Projectile shipProjObj (effect shipProj)

  {- processEnemyFire gamestate
     Gives a list of projectiles that are allowed in the current gamestate
     PRE: True
     RETURNS: A list of all current enemy projectiles
     EXAMPLES:
  -}

processEnemyFire :: Game -> [Projectile]
processEnemyFire gameState@(GameState {enemies=enemies,ticker=t}) = newProjList
  where
    newProjList = processEnemyFireAux (map (shipFire (-1,0) t) enemies) []

  {- processEnemyFireAux list1 list2
     Merges two lists of projectiles into one
     PRE: True
     VARIANT: length xs
     RETURNS: A new or unchanged list of projectiles
     EXAMPLES: processEnemyFireAux [proj1, proj2] [proj3, proj4] = [proj2, proj1     ,proj3, proj4]
  -}
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
           bounds = (0,0),
           graphic = testGraphic
         }

  

