module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import Debug.Trace
import Projectile
import DataTypes
import Globals
import Rendering
--import Control.Lens -- PLAN B: SOLVES NESTED RECORD FIELD HELL


window :: Display
window = InWindow win_title win_size win_offset

-- CHANGE THIS
playerObj :: Object
playerObj = Object { position = (0, 0),
                     direction = (0, 0),
                     speed = 200,
                     boundingBox = ((0, 0), (0, 0)),
                     graphic = rectangleSolid 50.0 50.0
                   }

initGameState :: Game
initGameState = GameState {
  objects = [],
  player = playerObj
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
  --display window win_background $ pictures [(color red $ makeRectangle (0,0) 50.0 50.0), (color black $ circle 10)]
  play window win_background targetFramerate initGameState draw handleEvent update

{- draw gameState
Constructs a drawable picture out of a given game state.
   PRE: 
   RETURNS:
   EXAMPLES: 
-}
draw :: Game -> Picture
draw gameState@(GameState {objects=objs, player=playerObj}) = pictures $ (map makeDrawable objs) ++ [player]
  where
    player = makeDrawable playerObj

{- update
desc
   PRE:
   RETURNS:
   EXAMPLES:
-}
update :: Float -> Game -> Game
update dt gameState = updatePlayer dt gameState

updatePlayer dt gameState@(GameState {player=ply}) = movePlayer gameState v
  where
    (dx,dy) = direction ply
    plySpeed = speed ply
    v = (dx*plySpeed*dt,dy*plySpeed*dt)


-- TODO: COLLISION DETECTION
checkCollision = undefined

testGraphic = translate (-25) 25 $ circle 30
testObject =
  Object { position = (-25,25),
           direction = (0, 0),
           speed = 0,
           boundingBox = ((-55, 55), (5, -5)),
           graphic = testGraphic
         }

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
    (SpecialKey KeySpace) -> undefined
    _ -> gameState
handleEvent (EventKey key Up _ _) gameState=
  case key of
    (SpecialKey KeyUp)    -> modPlyDirection gameState (0,-1)
    (SpecialKey KeyDown)  -> modPlyDirection gameState (0,1)
    (SpecialKey KeyLeft)  -> modPlyDirection gameState (1,0)
    (SpecialKey KeyRight) -> modPlyDirection gameState (-1,0)
    (SpecialKey KeySpace) -> undefined
    _ -> gameState
handleEvent _ gameState = gameState

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
