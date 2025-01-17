module Main where
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import Enemies
import Player
import Projectile
import DataTypes
import Presets
import Rendering
import Utilities
import Collision
import Encounter
import Test.HUnit
import Debug.Trace

-- The game window
window :: Display
window = InWindow winTitle winSize winOffset

{- main
   Initializes a game state and plays the game.
   PRE: True
   RETURNS: return ()
   SIDE EFFECTS: Initializes a game with a random number generator (generated with the global random number generator) and images loaded from disk.
   EXAMPLES: main
-}
main :: IO ()
main = do
  seed <- randomIO :: IO Int
  -- Load sprites
  gameGfx <- loadGfx
  -- Process sprites
  let gen = mkStdGen seed
      readyGameState = refreshGfx $ defaultGameState {gameGfx=gameGfx,randomGen=newGen, encounter=defaultEncounter}
      (generatedShipStack, newGen) =
        generateShips gen enemySpawnAmount (enemyTemplate readyGameState)
      readyEncounter = defaultEncounter {shipStack=generatedShipStack}
  play window winBackground targetFramerate (readyGameState {randomGen=newGen, encounter=readyEncounter }) draw handleEvent update
  return ()

{- newGame gameState
   Resets parts of a game state to starting values.
   PRE: True
   RETURNS: a new game state based on gameState with some of its values reset.
-}
newGame :: Game -> Game
newGame gameState@(GameState{randomGen=randomGen, enemyTemplate=enemyTemplate, player=player}) = gameState{encounter=initEncounter, player=newPlayer, enemyProjectiles=[], plyProjectiles=[], ticker=0, randomGen=newGen, enemies=[]}
  where
  (generatedShipStack, newGen) = generateShips randomGen enemySpawnAmount enemyTemplate
  initEncounter = defaultEncounter {shipStack=generatedShipStack}
  --Perserve player dir
  newPlayer = (plyTemplate gameState) {shipObj = newShipObj}
  newShipObj = (shipObj (plyTemplate gameState)) {direction = oldPlayerDir}
  oldPlayerDir = direction (shipObj (player))
  

{- draw gameState
   Constructs a drawable picture out of drawable game objects in a given game state.
   PRE: True
   RETURNS: A final picture based on everything that can and should be drawn in gameState.
   EXAMPLES: draw defaultGameState
   Example omitted (can't properly represent pictures in an example).
-}
draw :: Game -> Picture
draw gameState@(GameState {gameGfx=gameGfx, player=playerShip, plyProjectiles=plyProjs, enemyProjectiles=enemyProjs, enemies=enemies, showHitbox=showHitbox,backgroundFx=backgroundFx, encounter=encounter}) = newFrame
  where
    -- Everything that needs to be drawn goes here
    -- updateHealthDisplay is a bit code smelly, no time to fix it
    -- so the below is possibly the best (or least messy) solution for now.
    heartPics = map makeDrawable (updateHealthDisplay playerShip (heartGfx gameGfx) (gameOverGfx gameGfx))

    backgroundPic = makeDrawable backgroundFx
    objPics =
      (map projObj enemyProjs) ++ (map projObj plyProjs) ++ (map shipObj enemies) ++ (shipObj playerShip):[]
    finalObjPics = if showHitbox
      then (map makeDrawable objPics) ++ (map drawBounds objPics)
      else (map makeDrawable objPics)
    winScreenSprite = if (enemies == []) && ((shipStack encounter) == []) && (shipHealth playerShip) > 0
      then makeDrawable $ dummyObject {graphic = (winScreenGfx gameGfx)}
      else Blank
    -- The final picture frame
    newFrame = pictures $ backgroundPic:winScreenSprite:finalObjPics ++ heartPics

{- update deltaTime gameState
   Updates a given game state one iteration.
   PRE: True
   RETURNS: An updated game state based on gameState and deltaTime.
   EXAMPLES: update 0 defaultGameState == defaultGameState
-}
update :: Float -> Game -> Game
update dt gameState@(GameState {ticker=currentTick,plyProjectiles=projList, enemies=enemies,enemyProjectiles=enemyProjList,encounter=encounter, player=player,backgroundFx=backgroundFx}) = newGameState 
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
    newEnemies = updateEnemies dt gameState {enemies=spawnedEnemies}
    updatedEnemyProjList =
      map (updateProjectile dt) (colEnemProj gameState enemyProjList)
    newEnemyProjList = (processEnemyFire gameState) ++ updatedEnemyProjList
    
    -- Game related
    newTicker = currentTick+dt
    newBackgroundFx = scrollBackground dt backgroundFx
    
    -- The final updated gamestate
    newGameState = (gameState {player=newPlayer, ticker=newTicker, plyProjectiles=newPlyProjList, enemies=newEnemies, enemyProjectiles=newEnemyProjList, encounter=newEncounter, backgroundFx=newBackgroundFx})

{- handleEvent event gameState
   Modifies a game state based on an event.
   PRE: True
   RETURNS: A new game state based on gameState and event. The new game state might differ from gameState depending on what event transpired.
   EXAMPLES: handleEvent (EventResize (0,0)) defaultGameState == defaultGameState
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

{- scrollBackground deltaTime bgObj
   Scrolls some background object horizontally across the screen, moving it to the opposing side of the screen should it go outside the screen.
   PRE: True
   RETURNS: An object based on bgObj with a new position based on the deltaTime and backgroundScrollSpeed,
   or a new position based on the screen width if its horizontal position extends further than one screen width away from the center of the screen.
   EXAMPLES: scrollBackground 0 defaultBackground == defaultBackground
-}
scrollBackground :: Float -> Object -> Object
scrollBackground dt bgObj@(Object {position=(x,_)}) = newBackgroundObj
  where
    newBackgroundObj
      | x <= -winWidth = move (winWidth,0) bgObj
      | otherwise = move (-dt*backgroundScrollSpeed,0) bgObj
    
-- Test cases and test related functions go here for now
testGameState = defaultGameState -- this will change to more advanced test gamestates in the future

testGraphic = translate (-25) 25 $ circle 30
testObject =
  Object { position = (-25,25),
           direction = (0, 0),
           speed = 0,
           bounds = (0,0),
           graphic = testGraphic
         }

--TESTCASES AND TEST OBJECTS


--Collision
farObj = Object { position = (700, 0),
                  direction = (0, 0),
                  speed = 300,
                  bounds = (10, 10),
                  graphic = Blank
                }


smallObj = Object { position = (15, 0),
                    direction = (0, 0),
                    speed = 300,
                    bounds = (5, 5),
                    graphic = Blank
                  }

bigObj = Object { position = (0, 0),
                  direction = (0, 0),
                  speed = 300,
                  bounds = (10, 10),
                  graphic = Blank
                }
       
objPoint = Object { position = (0, 0),
                    direction = (0, 0),
                    speed = 300,
                    bounds = (0, 0),
                    graphic = Blank
                  }

testShip = Ship { shipObj = testShipObj,
                  wepCooldown = 1,
                  isFiring = False,
                  isPlayer = False,
                  shipHealth = 1,
                  lastFiredTick = 0,
                  projectile = testProj
                }

testShipObj = Object { position = (200, 200),
                       direction = (0, 0),
                       speed = 300,
                       bounds = (10, 10),
                       graphic = Blank
                     }


testProj = Projectile { projObj = testShipObj,
                        effect = Damage 1
                      }


-- Collision tests


test1 = TestCase $ assertEqual "No contact boxes" False (checkRectCollision farObj bigObj)
 
test2 = TestCase $ assertEqual "bordering bounding boxes" False (checkRectCollision smallObj bigObj)
 
test3 = TestCase $ assertEqual "One object is a point (no area of the boundings)" True (checkRectCollision bigObj objPoint)

test4 = TestCase $ assertEqual "Testing enemy damage despawn handling" [] (eneHandleDmg (defaultGameState {plyProjectiles = [testProj]}) [testShip])

test5 = TestCase $ assertEqual "Testing enemy damage despawn handling" [testShip] (eneHandleDmg (defaultGameState {plyProjectiles = [testProj]}) [testShip{shipHealth=2}])





 
