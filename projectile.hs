module Projectile where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import Debug.Trace
import DataTypes
import Helpers

testProj =
  Projectile { proj_obj = testProjObj,
               effect = NoEffect
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

spawnProjectile :: Projectile -> Ship -> Game -> Game
spawnProjectile proj owner@(Ship {isPlayer=isPlayer}) gameState
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
    
