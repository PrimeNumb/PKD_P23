module Player where
import DataTypes
import Graphics.Gloss
import Utilities
import Projectile
import Debug.Trace

playerObj :: Object
playerObj = Object { position = (0, 0),
                     direction = (0, 0),
                     speed = 300,
                     bounds = (25, 25),
                     graphic = color green $ rectangleSolid 50 50
                   }
playerShipDefault :: Ship
playerShipDefault = Ship { shipObj = playerObj,
                    shipHealth = 3,
                    wepCooldown = 0.25,
                    projectile = playerDefaultProj,
                    lastFiredTick = 0,
                    isFiring = False,
                    isPlayer = True
                  }
{- updatePlayer deltaTime gameState
   Updates the player one iteration.
   PRE: True
   RETURNS: A new game state where the player record field has been updated, partially based on the deltatime.
   EXAMPLES: Omitted
-}
updatePlayer :: Float -> Game -> Ship
updatePlayer dt gameState@(GameState {ticker=currentTick,player=ply}) =
  newPlayer
  where
    -- Update the last fired tick
    canFire = (currentTick - (lastFiredTick ply)) > (wepCooldown ply)
    updatedTick =
      case canFire of
        False -> lastFiredTick ply
        True  -> currentTick
    plyObj = shipObj ply
    -- Movement
    (dx,dy) = direction plyObj
    plySpeed = speed plyObj
    deltaPos = (dx*plySpeed*dt,dy*plySpeed*dt) --rename this
    
    -- The new player 
    newPlayer =
      ply { shipObj =
           (clampToBounds (playableBounds gameState) $ move plyObj deltaPos), lastFiredTick = updatedTick }
