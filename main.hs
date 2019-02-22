module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import Debug.Trace
import Projectile
import DataTypes
--import Control.Lens -- PLAN B: SOLVES NESTED RECORD FIELD HELL

-- INIT
win_title :: String
win_title = "Space Shooter"

win_size :: (Int, Int)
win_size = (640, 480)

win_offset :: (Int, Int)
win_offset = (0, 0)

win_background :: Color
win_background = white

targetFramerate :: Int
targetFramerate = 60

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
  Projectile.projGreet
  --let sampleCircle1 = translate 50 50 $ (circle 69)
  --    sampleCircle2 = (circle 69)
  --    toDraw = pictures [sampleCircle1, sampleCircle2]
  --display window win_background $ pictures [(color red $ makeRectangle (0,0) 50.0 50.0), (color black $ circle 10)]
  --play window win_background targetFramerate initGameState draw handleEvent update

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
    
    

{- makeDrawable
Converts a game object into a picture ready to be drawn on the screen.
   PRE: 
   RETURNS: 
   EXAMPLES: 
-}
makeDrawable :: Object -> Picture
makeDrawable (Object {position = pos, graphic=g}) = uncurry translate pos $ g

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


{- makeRectangle position width height
   desc
   PRE: 
   RETURNS: 
   EXAMPLES: 
-}
makeRectangle :: (Float, Float) -> Float -> Float -> Picture
makeRectangle point@(x, y) width height = polygon [upperLeft, upperRight, lowerRight, lowerLeft]
  where
    upperLeft = (x-(width/2), y+(height/2))
    upperRight = (x+(width/2), y+(height/2))
    lowerRight = (x+(width/2), y-(height/2))
    lowerLeft = (x-(width/2), y-(height/2))
