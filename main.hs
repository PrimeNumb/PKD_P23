module Main(main) where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random

-- Preliminary, subject to change
data Game = GameState
  { objects :: [Object],
    player :: Object
  } deriving Show

-- Preliminary, subject to change
data Object = Object
  { position :: (Float, Float),
    direction :: (Float, Float),
    velocity :: (Float, Float),
    boundingBox :: BoundingBox,
    graphic :: Picture
  } deriving Show

-- Preliminary, subject to change
data Ship = Ship
  { object :: Object,
    health :: Int,
    speed  :: Float
  } deriving Show

{- BoundingBox
   Represents a rectangle. The first element of the tuple is the 2D coordinate of the upper left corner of the rectangle. The second element of the tuple is the 2D coordinate of the lower right corner of the rectangle.
-}
type BoundingBox = ((Float, Float), (Float, Float))

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
                     velocity = (0, 0),
                     boundingBox = ((0, 0), (0, 0)),
                     graphic = rectangleSolid 50.0 50.0
                   }

initGameState :: Game
initGameState = GameState {
  objects = [],
  player = playerObj
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
  let sampleCircle1 = translate 50 50 $ (circle 69)
      sampleCircle2 = (circle 69)
      toDraw = pictures [sampleCircle1, sampleCircle2]
  display window win_background $ pictures [(color red $ makeRectangle (0,0) 50.0 50.0), (color black $ circle 10)]
  --play window win_background targetFramerate initGameState draw handleEvent update

{- draw gameState
Constructs a drawable picture out of a given game state.
   PRE: 
   RETURNS:
   EXAMPLES: 
-}
draw :: Game -> Picture
draw (GameState {objects=objs}) = pictures $ map makeDrawable objs

{- makeDrawable
Converts a game object into a picture that can be drawn on the screen.
   PRE: 
   RETURNS: 
   EXAMPLES: 
-}
makeDrawable :: Object -> Picture
makeDrawable (Object {position = pos, graphic=g}) = g

{- update
desc
   PRE:
   RETURNS:
   EXAMPLES:
-}
update :: Float -> Game -> Game
update dt gameState = gameState

testGraphic = translate (-25) 25 $ circle 30
testObject =
  Object { position = (-25,25),
           direction = (0, 0),
           velocity = (0, 0),
           boundingBox = ((-55, 55), (5, -5)),
           graphic = testGraphic
         }

{- handleEvent gameState
desc
   PRE:
   RETURNS:
   EXAMPLES:
-}
handleEvent :: Event -> Game -> Game
handleEvent (EventKey (key) Down modifier _ ) gameState =
  case key of
    (SpecialKey KeyUp)    -> moveObject 10.0 gameState (0, 1)
    (SpecialKey KeyDown)  -> gameState
    (SpecialKey KeyLeft)  -> gameState 
    (SpecialKey KeyRight) -> gameState 
    _                     -> gameState
handleEvent _ gameState = gameState

moveObject :: Float -> Game -> (Float, Float) -> Game
moveObject deltaTime gameState@(GameState { player = pObject }) direction = gameState { player = newPlayer }
  where
    (x, y) = position pObject
    (nx, ny) = (x, (y+5*deltaTime))
    newPlayer = pObject { position = (nx, ny) }


makeRectangle :: (Float, Float) -> Float -> Float -> Picture
makeRectangle point@(x, y) width height = polygon [upperLeft, upperRight, lowerRight, lowerLeft]
  where
    upperLeft = (x-(width/2), y+(height/2))
    upperRight = (x+(width/2), y+(height/2))
    lowerRight = (x+(width/2), y-(height/2))
    lowerLeft = (x-(width/2), y-(height/2))
