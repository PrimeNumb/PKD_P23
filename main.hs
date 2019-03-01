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

-- CHANGE THIS


defaultEncounter = Encounter
  { popInterval = enemySpawnInterval,
    lastPop = enemySpawnInitialDelay,
    shipStack = []
  }

defaultGameGFX = GameGFX
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
  gameGfx = defaultGameGFX,
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
  gameGFX <- loadGFX
  -- Process sprites
  let gen = mkStdGen seed
      readyGameState = refreshGFX $ initGameState {gameGfx=gameGFX,randomGen=newGen, encounter=defaultEncounter}
      (generatedShipStack, newGen) =
        generateEncounter gen 10 (enmyTemplate readyGameState)
      readyEncounter = defaultEncounter {shipStack=generatedShipStack}
  play window winBackground targetFramerate (readyGameState {randomGen=newGen, encounter=readyEncounter }) draw handleEvent update
  return ()

{-newGame gameState
Takes in the current GameState and resets it to default values allowing the player to start over.
PRE:
RETURNS: A new game state with reset values.
-}
newGame :: Game -> Game
newGame gameState@(GameState{randomGen=randomGen, enmyTemplate=enmyTemplate}) = gameState{encounter=initEncounter, player=(plyTemplate gameState), enemyProjectiles=[], plyProjectiles=[], ticker=0, randomGen=newGen, objects=[], enemies=[]}
  where
  (generatedShipStack, newGen) = generateEncounter randomGen 10 enmyTemplate
  initEncounter = defaultEncounter {shipStack=generatedShipStack}

refreshGFX :: Game -> Game
refreshGFX gameState@(GameState {gameGfx=gameGFX}) = newGameState
  where
    newPlyProj =
      setSprite (projectile $ player gameState) (playerProjGfx gameGFX)
    newPlayerTemplate =
      (setSprite (player gameState) (playerGfx gameGFX)) { projectile = newPlyProj }
    newEnmyProj =
      setSprite (projectile $ enmyTemplate gameState) (enemyProjGfx gameGFX)
    newEnmyTemplate =
      (setSprite (enmyTemplate gameState) (enemyStandardGfx gameGFX)) { projectile = newEnmyProj }
    newGameState = gameState
      {
        player = newPlayerTemplate,
        plyTemplate = newPlayerTemplate,
        enmyTemplate = newEnmyTemplate,
        plyProjTemplate = setSprite (plyProjTemplate gameState) (playerProjGfx gameGFX),
        enmyProjTemplate = setSprite (enmyProjTemplate gameState) (enemyProjGfx gameGFX),
        background = setSprite (background gameState) (backgroundGfx gameGFX)
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
          playerGfx = playerGFX,
          enemyStandardGfx = enemyStandardGFX,
          playerProjGfx = playerProjGFX,
          enemyProjGfx = enemyProjGFX,
          heartGfx = heartGFX,
          gameOverGfx = gameOverGFX,
          backgroundGfx = backgroundGFX
        }
  return gameGFX

processSprite :: Maybe Picture -> Picture
processSprite Nothing = color green $ rectangleSolid 50 50
processSprite (Just pic) = pic

draw :: Game -> Picture
draw gameState@(GameState {objects=objs, gameGfx=gameGFX, player=playerShip, plyProjectiles=plyProjs, enemyProjectiles=enemyProjs, enemies=enemies, showHitbox=showHitbox,background=background}) = newFrame
  where
    -- Everything that needs to be drawn goes here
    heartPics = map makeDrawable (updateHealthDisplay playerShip (heartGfx gameGFX) (gameOverGfx gameGFX))
    backgroundPic = makeDrawable background
    drawObjs =
      (map projObj enemyProjs) ++ (map projObj plyProjs) ++ (map shipObj enemies) ++ (shipObj playerShip):objs
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
updateHealthDisplay player@(Ship{shipHealth=shipHealth}) heartGFX gameOverGFX
  |shipHealth == -1 = [Object { position = (0, 0),
                                direction = (0, 0),
                                speed = 0,
                                bounds = (0, 0),
                                graphic = gameOverGFX
                               }]
  |shipHealth <= 0 = []
  |otherwise = (Object { position = (xpos, 250),
                         direction = (0, 0),
                         speed = 0,
                         bounds = (0, 0),
                         graphic = heartGFX
                       }) : updateHealthDisplay newShip heartGFX gameOverGFX
  where
    newShip = player {shipHealth=newHp}
    newHp = shipHealth - 1
    xpos = fromIntegral (-500 + (40 * shipHealth))
  
updateEnemies :: [Ship] -> Float -> Game -> [Ship]
updateEnemies enemies dt gameState = map (updateEnemy dt gameState) (eneHandleDmg gameState enemies)

{- handleEvent gameState
Calls a specific
   PRE:
   RETURNS:
   EXAMPLES:
-}
handleEvent :: Event -> Game -> Game
handleEvent (EventKey key Down mod _) gameState@(GameState {player=player}) =
  case key of
    (SpecialKey KeyUp)    -> gameState { player =  modDirection player(0,1)}
    (SpecialKey KeyDown)  -> gameState { player =  modDirection player(0,-1)}
    (SpecialKey KeyLeft)  -> gameState { player =  modDirection player(-1,0)}
    (SpecialKey KeyRight) -> gameState { player =  modDirection player(1,0)}
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
    (SpecialKey KeyUp)    -> gameState { player =  modDirection player(0,-1)}
    (SpecialKey KeyDown)  -> gameState { player =  modDirection player(0,1)}
    (SpecialKey KeyLeft)  -> gameState { player =  modDirection player(1,0)}
    (SpecialKey KeyRight) -> gameState { player =  modDirection player(-1,0)}
    (SpecialKey KeySpace) -> gameState { player = player {isFiring=False}}
    _ -> gameState


    
-- Need to do something when the screen is resized
handleEvent (EventResize (x, y)) gameState = gameState
handleEvent _ gameState = gameState

-- Fires a ship's projectile from its position, given a direction
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


processEnemyFire :: Game -> [Projectile]
processEnemyFire gameState@(GameState {enemies=enemies,ticker=t}) = newProjList
  where
    newProjList = processEnemyFireAux (map (shipFire (-1,0) t) enemies) []

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

  

