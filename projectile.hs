module Projectile where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import Debug.Trace
import DataTypes
import Utilities
import Presets

  
{- updateProjectile deltaTime proj1
   Updates a projectile one iteration.
   PRE: deltaTime >= 0
   RETURNS: An updated projectile based on deltaTime and proj1.
   EXAMPLES: updateProjectile 1.0 enemyDefaultProj == enemyDefaultProj {projObj=(projObj enemyDefaultProj) {position=(-600,0)}}
-}  
updateProjectile :: Float -> Projectile -> Projectile
updateProjectile dt proj@(Projectile {projObj=pObj}) = newProj
  where
    pSpeed = speed pObj
    (dx, dy) = direction pObj
    newProj = proj {projObj = move (dx*pSpeed*dt,dy*pSpeed*dt) (projObj proj)}
