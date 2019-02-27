module DataTypes where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import Debug.Trace

-- Preliminary, subject to change
data Game = GameState
  { playable_bounds :: BoundingBox,
    objects         :: [Object], --use this for objects that aren't ships
    enemies         :: [Ship],
    encounterStack  :: EncounterStack,
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

data EncounterStack =
  EncounterStack { pop_interval  :: Float,
                   last_pop      :: Float,
                   ship_stack    :: [Ship]
                   } deriving Show
data Effect = Damage Int | NoEffect deriving Show

{- BoundingBox
   Represents a rectangle.
   The first element of the tuple is the 2D coordinate of the upper left corner of the rectangle.
   The second element of the tuple is the 2D coordinate of the lower right corner of the rectangle.
-}
type BoundingBox = (Float, Float)
type Position = (Float, Float)
type Direction = (Float, Float)



-- TYPECLASS DOCS GO HERE
class Movable a where
  move :: a -> Vector -> a
  setPos :: Vector -> a -> a

instance Movable Object where
  move obj@(Object {position=(x,y)}) (vx,vy) = obj { position = (x+vx,y+vy) }
  setPos (x,y) obj@(Object {position=(obj_x,obj_y)}) =
    move obj (x-obj_x,y-obj_y)
instance Movable Ship where
  move ship@(Ship {ship_obj=obj}) v = ship {ship_obj=(move obj v)}
  setPos pos ship@(Ship {ship_obj=obj}) =
    ship {ship_obj=(setPos pos obj)}

instance Movable Projectile where
  move proj@(Projectile {proj_obj=obj}) v = proj {proj_obj=(move obj v)}
  setPos pos proj@(Projectile {proj_obj=obj}) =
    proj {proj_obj=(setPos pos obj)}
class Drawable a where
  makeDrawable :: a -> Picture
  drawBounds :: a -> Picture
  drawWithBounds :: a -> Picture

instance Drawable Object where
  makeDrawable (Object {position = pos, graphic=g}) =
    uncurry translate pos $ g
  drawBounds (Object {position=(x,y), boundingBox=(bx,by)}) =
    color red $ translate x y $ rectangleWire (2*bx) (2*by)
  drawWithBounds obj@(Object {position=(x,y), boundingBox=(bx,by)}) =
    pictures $ (makeDrawable obj):(drawBounds obj):[]
    
instance Drawable Ship where
  makeDrawable (Ship {ship_obj=obj}) = makeDrawable obj
  drawBounds (Ship {ship_obj=obj}) = drawBounds obj
  drawWithBounds (Ship {ship_obj=obj}) = drawWithBounds obj

instance Drawable Projectile where
  makeDrawable (Projectile {proj_obj=obj}) = makeDrawable obj
  drawBounds (Projectile {proj_obj=obj}) = drawBounds obj
  drawWithBounds (Projectile {proj_obj=obj}) = drawWithBounds obj
