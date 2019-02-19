module Main(main) where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random

-- Preliminary, subject to change
data Game = GameState
  { sprites :: [Sprite]
  } deriving Show

-- Preliminary, subject to change
data Sprite = Sprite
  { position :: (Float, Float),
    boundingBox :: BoundingBox,
    graphic :: Picture
  } deriving Show

-- Preliminary, subject to change
data Ship = Ship
  { sprite :: Sprite,
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

initGameState :: Game
initGameState = GameState []


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
      sampleCircle2 = translate (-15) 20 $ (circle 25)
      toDraw = pictures [sampleCircle1, sampleCircle2]
  play window win_background targetFramerate initGameState draw handleEvent update

{- draw gameState
Constructs a drawable picture out of a given game state.
   PRE: 
   RETURNS:
   EXAMPLES: 
-}
draw :: Game -> Picture
draw (GameState {sprites=sprites}) = pictures $ map makeDrawable sprites

{- makeDrawable
Converts a sprite into a picture that can be drawn on the screen.
   PRE: 
   RETURNS: 
   EXAMPLES: 
-}
makeDrawable :: Sprite -> Picture
makeDrawable (Sprite {graphic=g}) = g

{- update
desc
   PRE:
   RETURNS:
   EXAMPLES:
-}
update :: Float -> Game -> Game
update _ gameState = gameState

testGraphic = translate (-25) 25 $ circle 30
testSprite =
  Sprite { position = (-25,25),
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
    (SpecialKey KeyUp)    -> GameState { sprites = [testSprite] }
    (SpecialKey KeyDown)  -> GameState { sprites = [] }
    (SpecialKey KeyLeft)  -> GameState { sprites = [] }
    (SpecialKey KeyRight) -> GameState { sprites = [] }
handleEvent _ gameState = gameState
