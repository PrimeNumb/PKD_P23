module Projectile where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import Debug.Trace
import DataTypes
import Utilities
import Globals

-- The default projectile speed
projObjDefaultSpeed :: Float
projObjDefaultSpeed = 600

--The default projectile bounds
projObjDefaultBounds :: Bounds
projObjDefaultBounds = (2.5,2.5)

--The default projectile graphic
projObjDefaultGfx :: Picture
projObjDefaultGfx = color red $ circleSolid 5

--The default player projectile object
playerDefaultProjObj =
  Object { position = (0,0),
           direction = (1,0),
           speed = projObjDefaultSpeed,
           bounds = projObjDefaultBounds,
           graphic = projObjDefaultGfx
         }

--The default player projectile damage
playerDefaultProj = Projectile playerDefaultProjObj (Damage 1)


--The default enemy projectile object
enemyDefaultProjObj =
  Object { position = (0,0),
           direction = (-1,0),
           speed = projObjDefaultSpeed,
           bounds = (16.5,4.5),
           graphic = projObjDefaultGfx
         }

--The default enemy projectile damage
enemyDefaultProj = Projectile enemyDefaultProjObj (Damage 1)


--The post-game ship (invisShip) projectile object
harmlessProjObj = Object { position = (0,0),
                        direction = (-1,0),
                        speed = 0,
                        bounds = (0,0),
                        graphic = rectangleSolid 1 1

                         }

--The post-game ship (invisShip) projectile damage
harmlessProj = Projectile harmlessProjObj (Damage 0)


-- Adds a list of projectiles into the gamestate based on whether
-- they belong to the player or not


spawnProjectiles :: [Projectile] -> Bool -> Game -> Game
spawnProjectiles projList isPlayer gameState@(GameState {plyProjectiles=plyProjList, enemyProjectiles=npcProjList})
  | isPlayer = gameState { plyProjectiles = (projList ++ plyProjList)}
  | otherwise = gameState { enemyProjectiles = (projList ++ npcProjList)}
  
-- Adds a projectile into the gamestate based on whether they
-- belong to the player or not
spawnProjectile :: Projectile -> Bool -> Game -> Game
spawnProjectile proj isPlayer gameState
  | isPlayer = gameState { plyProjectiles = plyProjList }
  | otherwise = gameState { enemyProjectiles = npcProjList }
  where
    plyProjList = proj:(plyProjectiles gameState)
    npcProjList = proj:(enemyProjectiles gameState)



  {- updateProjectile deltaTime proj1
     Updates a projectile.
     PRE: dt >= 0
     RETURNS: An updated projectile based on deltaTime and proj1.
     EXAMPLES:
  -}
  
updateProjectile :: Float -> Projectile -> Projectile
updateProjectile dt proj@(Projectile {projObj=pObj}) = newProj
  where
    pSpeed = speed pObj
    (dx, dy) = direction pObj
    newProj = proj {projObj = move (dx*pSpeed*dt,dy*pSpeed*dt) (projObj proj)}
    





{-
testProj =
  Projectile { projObj = testProjObj,
               effect = Damage 1
             }
testProjObj =
  Object { position = (0,0),
           direction = (1,0),
           speed = 100,
           bounds = (0,0),
           graphic = testProjGraphic
         }

testProjGraphic = color blue $ circleSolid 5
-}
