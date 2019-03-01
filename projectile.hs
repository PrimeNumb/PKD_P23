module Projectile where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import Debug.Trace
import DataTypes
import Utilities
import Globals

-- Projectile templates
projObjDefaultSpeed :: Float
projObjDefaultSpeed = 600

projObjDefaultBounds :: Bounds
projObjDefaultBounds = (2.5,2.5)

projObjDefaultGfx :: Picture
projObjDefaultGfx = color red $ circleSolid 5

playerDefaultProjObj =
  Object { position = (0,0),
           direction = (1,0),
           speed = projObjDefaultSpeed,
           bounds = projObjDefaultBounds,
           graphic = projObjDefaultGfx
         }
playerDefaultProj = Projectile playerDefaultProjObj (Damage 1)


enemyDefaultProjObj =
  Object { position = (0,0),
           direction = (-1,0),
           speed = projObjDefaultSpeed,
           bounds = (16.5,4.5),
           graphic = projObjDefaultGfx
         }
enemyDefaultProj = Projectile enemyDefaultProjObj (Damage 1)

harmlessProjObj = Object { position = (0,0),
                        direction = (-1,0),
                        speed = 0,
                        bounds = (0,0),
                        graphic = rectangleSolid 1 1
                      }
harmlessProj = Projectile harmlessProjObj (Damage 0)


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

-- Adds a list of projectiles into the gamestate based on whether
-- they belong to the player or not

  {- spawnProjectiles ListOfProjs PlayerCheck Gamestate
     
     PRE:  ... pre-condition on the arguments, if any ...
     RETURNS: ... description of the result, in terms of the arguments ...
     SIDE EFFECTS: ... side effects, if any, including exceptions ...
     EXAMPLES: ... especially if useful to highlight delicate issues; also consider including counter-examples ...
  -}
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

updateProjectile :: Float -> Projectile -> Projectile
updateProjectile dt proj@(Projectile {projObj=pObj}) = newProj
  where
    pSpeed = speed pObj
    (dx, dy) = direction pObj
    newProj = move proj (dx*pSpeed*dt,dy*pSpeed*dt)
    
