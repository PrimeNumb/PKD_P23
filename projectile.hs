module Projectile where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import Debug.Trace
import DataTypes
import Utilities
import Globals

-- Projectile templates
projObjDefault_spd :: Float
projObjDefault_spd = 600

projObjDefault_bounds :: Bounds
projObjDefault_bounds = (2.5,2.5)

projObjDefault_gfx :: Picture
projObjDefault_gfx = color red $ circleSolid 5

playerDefaultProjObj =
  Object { position = (0,0),
           direction = (1,0),
           speed = projObjDefault_spd,
           bounds = projObjDefault_bounds,
           graphic = projObjDefault_gfx
         }
playerDefaultProj = Projectile playerDefaultProjObj (Damage 1)


enemyDefaultProjObj =
  Object { position = (0,0),
           direction = (-1,0),
           speed = projObjDefault_spd,
           bounds = (16.5,4.5),
           graphic = projObjDefault_gfx
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
  Projectile { proj_obj = testProjObj,
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
spawnProjectiles :: [Projectile] -> Bool -> Game -> Game
spawnProjectiles projList isPlayer gameState@(GameState {ply_projectiles=plyProjList, npc_projectiles=npcProjList})
  | isPlayer = gameState { ply_projectiles = (projList ++ plyProjList)}
  | otherwise = gameState { npc_projectiles = (projList ++ npcProjList)}
  
-- Adds a projectile into the gamestate based on whether they
-- belong to the player or not
spawnProjectile :: Projectile -> Bool -> Game -> Game
spawnProjectile proj isPlayer gameState
  | isPlayer = gameState { ply_projectiles = plyProjList }
  | otherwise = gameState { npc_projectiles = npcProjList }
  where
    plyProjList = proj:(ply_projectiles gameState)
    npcProjList = proj:(npc_projectiles gameState)

updateProjectile :: Float -> Projectile -> Projectile
updateProjectile dt proj@(Projectile {proj_obj=pObj}) = newProj
  where
    pSpeed = speed pObj
    (dx, dy) = direction pObj
    newProj = move proj (dx*pSpeed*dt,dy*pSpeed*dt)
    
