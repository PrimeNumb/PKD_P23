module DataTypes where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import Debug.Trace

{- Game


   INVARIANT: 
-}
data Game = GameState
  { objects          :: [Object], --use this for objects that aren't ships
    game_gfx         :: GameGFX,
    enemies          :: [Ship],
    playable_bounds  :: Bounds,
    randomGen        :: StdGen,
    encounter        :: Encounter,
    player           :: Ship,
    ply_projectiles  :: [Projectile],
    npc_projectiles  :: [Projectile],
    ticker           :: Float,
    background       :: Object,
    plyTemplate      :: Ship,
    enmyTemplate     :: Ship,
    plyProjTemplate  :: Projectile,
    enmyProjTemplate :: Projectile,
    showHitbox       :: Bool
  } deriving Show

{- Object


   INVARIANT: 
-}
data Object = Object
  { position    :: (Float, Float),
    direction   :: (Float, Float),
    speed       :: Float,
    bounds      :: Bounds,
    graphic     :: Picture
  } deriving Show

{- Ship


   INVARIANT: 
-}
data Ship = Ship
  { ship_obj        :: Object,
    ship_health     :: Int,
    wep_cooldown    :: Float,
    projectile      :: Projectile,
    last_fired_tick :: Float,
    isFiring        :: Bool,
    isPlayer        :: Bool
  } deriving Show

{- Projectile represents a (not necessarily) moving projectile in the game. 
   proj_obj describes the object associated with the Projectile.
   effect describes the effect that should be processed on a colliding object.
  INVARIANT: True
   -}
data Projectile = Projectile
  { proj_obj :: Object,
    effect   :: Effect
  } deriving Show

{- Encounter describes a set of ships that should be generated into the game and how often.
  pop_interval describes the interval with which a ship should be generated from the ship stack into the game.
  last_pop is the time (in game ticks) since the last pop of the stack; the time since a ship was last generated into the game.
  ship_stack is the set of ships that the encounter consists of.
  INVARIANT: pop_interval > 0
   -}
data Encounter = Encounter
  {
    pop_interval  :: Float,
    last_pop      :: Float,
    ship_stack    :: [Ship]
  } deriving Show

{- GameGFX represents a set of pictures (or images) used by different parts of a game. 
  player_gfx is the player sprite.
  enemy_standard_gfx is the default enemy sprite.
  player_proj_gfx is the player projectile sprite.
  enemy_proj_gfx is the enemy projectile sprite
  heart_gfx is the heart sprite.
  gameOver_gfx is the game over sprite.
  background_gfx is the background image.
  INVARIANT: True
   -}
data GameGFX = GameGFX
  {
    player_gfx         :: Picture,
    enemy_standard_gfx :: Picture,
    player_proj_gfx    :: Picture,
    enemy_proj_gfx     :: Picture,
    heart_gfx          :: Picture,
    gameOver_gfx       :: Picture,
    background_gfx     :: Picture
  } deriving Show

{- Effect represents how something should be affected when the effect is processed and applied to something in the game.
   Damage x, where x is the amount of damage that should be applied (including negative damage)
   NoEffect, where NoEffect is simply a lack of effect.
   INVARIANT: True
-}
data Effect = Damage Int | NoEffect deriving Show

{- Bounds represents the (rectangular) bounds around some point, and describes how far the bounds extend from that point in two dimensions.
   The first element of the tuple is the width with which the bounds extend in both directions of the x-coordinate plane, meaning the width * 2 would be the width of a bounding box.
   The second element of the tuple is height with which the bounds extend in both directions of the y-coordinate plane, meaning the height * 2 would be the height of a bounding box.
   INVARIANT: Both elements must be greater than 0
-}
type Bounds = (Float, Float)
{- Position represents a 2D coordinate in a two dimensional plane.
   The first element of the tuple is the x-coordinate.
   The second element of the tuple is the y-coordinate.
   INVARIANT: True
-}
type Position = (Float, Float)
{- Direction represents a direction in 2D space, or simply a velocity vector with no magnitude.
   The first element of the tuple is the direction on the x-axis, where negative values can be treated as a direction pointing to the left, and the reverse for positive values.
   The second element of the tuple is the direction on the y-axis, where negative values can be treated as a downward direction, and the reverse for positive values.
   A direction (1,1) would be treated as a direction pointing north-east.
   INVARIANT: Either element's value must be between -1.0 and 1.0.
-}
type Direction = (Float, Float)



-- TYPECLASS DOCS GO HERE
class Movable a where
  move :: a -> Vector -> a
  setPos :: Vector -> a -> a
  modDirection :: a -> Direction -> a
  
instance Movable Object where
  move obj@(Object {position=(x,y)}) (vx,vy) = obj { position = (x+vx,y+vy) }
  setPos (x,y) obj@(Object {position=(obj_x,obj_y)}) =
    move obj (x-obj_x,y-obj_y)
  modDirection obj@(Object {direction=(x,y)}) (dx, dy) =
    obj { direction = (x+dx,y+dy)}
instance Movable Ship where
  move ship@(Ship {ship_obj=obj}) v = ship {ship_obj=(move obj v)}
  setPos pos ship@(Ship {ship_obj=obj}) =
    ship {ship_obj=(setPos pos obj)}
  modDirection ship@(Ship {ship_obj=obj}) dir =
    ship {ship_obj = (modDirection obj dir )}
instance Movable Projectile where
  move proj@(Projectile {proj_obj=obj}) v = proj {proj_obj=(move obj v)}
  setPos pos proj@(Projectile {proj_obj=obj}) =
    proj {proj_obj=(setPos pos obj)}
  modDirection proj@(Projectile {proj_obj=obj}) dir =
    proj {proj_obj = (modDirection obj dir)}


class Drawable a where
  makeDrawable :: a -> Picture
  drawBounds :: a -> Picture
  drawWithBounds :: a -> Picture
  setSprite :: a -> Picture -> a

instance Drawable Object where
  makeDrawable (Object {position = pos, graphic=g}) =
    uncurry translate pos $ g
  drawBounds (Object {position=(x,y), bounds=(bx,by)}) =
    color red $ translate x y $ rectangleWire (2*bx) (2*by)
  drawWithBounds obj@(Object {position=(x,y), bounds=(bx,by)}) =
    pictures $ (makeDrawable obj):(drawBounds obj):[]
  setSprite obj gfx = obj {graphic=gfx}
    
instance Drawable Ship where
  makeDrawable (Ship {ship_obj=obj}) = makeDrawable obj
  drawBounds (Ship {ship_obj=obj}) = drawBounds obj
  drawWithBounds (Ship {ship_obj=obj}) = drawWithBounds obj
  setSprite ship@(Ship {ship_obj=obj}) gfx =
    ship {ship_obj=(setSprite obj gfx)}

instance Drawable Projectile where
  makeDrawable (Projectile {proj_obj=obj}) = makeDrawable obj
  drawBounds (Projectile {proj_obj=obj}) = drawBounds obj
  drawWithBounds (Projectile {proj_obj=obj}) = drawWithBounds obj
  setSprite proj@(Projectile {proj_obj=obj}) gfx =
    proj {proj_obj=(setSprite obj gfx)}
