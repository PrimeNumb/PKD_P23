module DataTypes where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import Debug.Trace

-- Preliminary, subject to change
data Game = GameState
  { objects :: [Object],
    player :: Object
    --pressedKeys :: [Key]
  } deriving Show

-- Preliminary, subject to change
data Object = Object
  { position :: (Float, Float),
    direction :: (Float, Float),
    speed :: Float,
    boundingBox :: BoundingBox,
    graphic :: Picture
  } deriving Show

-- Preliminary, subject to change
data Ship = Ship
  { ship_obj :: Object,
    health :: Int
  } deriving Show

data Projectile = Projectile
  { proj_obj :: Object,
    effect :: Effect
    }

data Effect = Damage Int | NoEffect

{- BoundingBox
   Represents a rectangle. The first element of the tuple is the 2D coordinate of the upper left corner of the rectangle. The second element of the tuple is the 2D coordinate of the lower right corner of the rectangle.
-}
type BoundingBox = ((Float, Float), (Float, Float))
