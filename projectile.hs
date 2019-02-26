module Projectile where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import Debug.Trace
import DataTypes
import Helpers

-- Projectile templates
projObjDefault_spd :: Float
projObjDefault_spd = 400

projObjDefault_bbox :: BoundingBox
projObjDefault_bbox = (2.5,2.5)

projObjDefault_gfx :: Picture
projObjDefault_gfx = color red $ circleSolid 5


testProj =
  Projectile { proj_obj = testProjObj,
               effect = Damage 1
             }
testProjObj =
  Object { position = (0,0),
           direction = (1,0),
           speed = 100,
           boundingBox = (0,0),
           graphic = testProjGraphic
         }

testProjGraphic = color blue $ circleSolid 5

-- Spawns a player projectile
--DEPRECATED - DO NOT USE
--spawnPlyProjectile :: Object -> Effect -> Game -> Game
--spawnPlyProjectile obj fx gameState
--  | (direction obj) == (0,0) = gameState { ply_projectiles = (Projectile newObj fx):projList }
--  | otherwise = gameState { ply_projectiles = (Projectile obj fx):projList }
--  where
--    newObj = obj { direction = (1,0) }
--    projList = ply_projectiles gameState
--    newProjList = (Projectile obj fx):projList

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
updateProjectile dt proj@(Projectile {proj_obj=pObj}) = proj { proj_obj = newProjObj}
  where
    testProjs = [] :: [Projectile]
    pSpeed = speed pObj
    (dx, dy) = direction pObj
    newProjObj = moveObject pObj (dx*pSpeed*dt,dy*pSpeed*dt)
    
