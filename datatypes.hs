module DataTypes where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import Debug.Trace

-- Preliminary, subject to change
data Game = GameState
  { objects         :: [Object], --use this for objects that aren't ships
    enemies         :: [Ship],
    player          :: Ship,
    ply_projectiles :: [Projectile],
    npc_projectiles :: [Projectile],
    ticker          :: Float,
    playerIsFiring  :: Bool -- move to Ship datatype?
  } deriving Show

-- Preliminary, subject to change
data Object = Object
  { position    :: (Float, Float),
    direction   :: (Float, Float),
    speed       :: Float,
    boundingBox :: BoundingBox,
    graphic     :: Picture
  } deriving Show

-- Preliminary, subject to change
data Ship = Ship
  { ship_obj        :: Object,
    ship_health     :: Int,
    wep_cooldown    :: Float,
    projectile      :: Projectile,
    last_fired_tick :: Float,
    isFiring        :: Bool,
    isPlayer        :: Bool
  } deriving Show

data Projectile = Projectile
  { proj_obj :: Object,
    effect   :: Effect
  } deriving Show

class Drawable a where
  makeDrawable :: a -> Picture

instance Drawable Object where
  makeDrawable (Object {position = pos, graphic=g}) = uncurry translate pos $ g
instance Drawable Ship where
  makeDrawable (Ship {ship_obj=obj}) = makeDrawable obj

instance Drawable Projectile where
  makeDrawable (Projectile {proj_obj=obj}) = makeDrawable obj

data Encounter = EncounterQueue [Ship]
data Effect = Damage Int | NoEffect deriving Show

{- BoundingBox
   Represents a rectangle.
   The first element of the tuple is the 2D coordinate of the upper left corner of the rectangle.
   The second element of the tuple is the 2D coordinate of the lower right corner of the rectangle.
-}
type BoundingBox = (Float, Float)
type Position = (Float, Float)
type Direction = (Float, Float)

