module Player where
import DataTypes
import Graphics.Gloss
import Utilities
import Projectile
import Debug.Trace
import Presets


{-updateHealthDisplay ship heartGFX gameOverGFX
  Updates the health display of the Ship so it correlates with current health. Also displays a game over graphic when the player is dead.
  PRE:
  RETURNS: A list of the objects that are to be drawn. Either hearts corresponding to Ship health or a game over graphic.
  EXAMPLES:
-}
updateHealthDisplay :: Ship -> Picture -> Picture -> [Object]
--VARIANT: shipHealth ship
updateHealthDisplay player@(Ship{shipHealth=shipHealth}) heartGfx gameOverGfx
  |shipHealth == -1 = [Object { position = (0, 0),
                                direction = (0, 0),
                                speed = 0,
                                bounds = (0, 0),
                                graphic = gameOverGfx
                               }]
  |shipHealth <= 0 = []
  |otherwise = (Object { position = (xpos, 250),
                         direction = (0, 0),
                         speed = 0,
                         bounds = (0, 0),
                         graphic = heartGfx
                       }) : updateHealthDisplay newShip heartGfx gameOverGfx
  where
    newShip = player {shipHealth=newHp}
    newHp = shipHealth - 1
    xpos = fromIntegral (-500 + (40 * shipHealth))


{- updatePlayer deltaTime gameState
   Updates the player one iteration.
   PRE: True
   RETURNS: A new player ship based on the player ship in gameState, where its properties have been updated based (partially) on deltaTime.
   EXAMPLES: updatePlayer 0.16 defaultGameState == playerDefaultShip
-}
updatePlayer :: Float -> Game -> Ship
updatePlayer dt gameState@(GameState {ticker=currentTick,player=ply}) = newPlayer
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
    traceStr1 = show (dx*sqrt(plySpeed^2 - dy^2))
    traceStr2 = show (dy*sqrt(plySpeed^2 - dx^2))
    deltaPos = (dx*sqrt(plySpeed^2 - dy^2)*dt,dy*sqrt(plySpeed^2 - dx^2)*dt) --rename this
    
    -- The new player 
    newPlayer =
      ply { shipObj =
           (clampToBounds (bounds $ background gameState) $ move deltaPos plyObj), lastFiredTick = updatedTick }
