module Enemies where
import DataTypes
import Utilities
import Presets
import Collision


{- processEnemyFire gamestate
   Processes whether all active enemies in a given game state should fire their weapons.
   PRE: True
   RETURNS: A list of fired enemy projectiles.
   EXAMPLES: processEnemyFire defaultGameState == []
-}
processEnemyFire :: Game -> [Projectile]
processEnemyFire gameState@(GameState {enemies=enemies,ticker=t}) = newProjList
  where
    newProjList = processEnemyFireAux (map (shipFire (-1,0) t) enemies) []

{- processEnemyFireAux maybeProjs acc
   Processes a list of potential projectiles.
   PRE: True
   RETURNS: The accumulator acc with the Just elements from maybeProjs prepended to acc.
   EXAMPLES: processEnemyFireAux [] [playerDefaultProj] == [playerDefaultProj]
-}
processEnemyFireAux :: [Maybe Projectile] -> [Projectile] -> [Projectile]
--VARIANT: length of xs
processEnemyFireAux [] acc = acc
processEnemyFireAux (Just x : xs) acc = processEnemyFireAux xs (x:acc)
processEnemyFireAux (Nothing : xs) acc = processEnemyFireAux xs acc


{- updateEnemies deltaTime gameState
   Updates the list of active enemies in a game state.
   PRE: True
   RETURNS: A list of updated enemies based on the list of enemies in gameStateand deltaTime.
   EXAMPLES: updateEnemies ()
-}
updateEnemies :: Float -> Game -> [Ship]
updateEnemies dt gameState = map (updateEnemy dt gameState) (eneHandleDmg gameState (enemies gameState))

{- updateEnemy deltaTime gameState ship1
   Updates an enemy one iteration.
   PRE: deltaTime >= 0
   RETURNS: A ship based on ship1 with updated properties based on deltaTime & gameState.
   EXAMPLES: updateEnemy 1.0 defaultGameState enemyShipDefaultTemplate ==
   enemyShipDefaultTemplate {shipObj=(shipObj enemyShipDefaultTemplate) {position=(949,0)},isFiring=False}
-}
updateEnemy :: Float -> Game -> Ship -> Ship               
updateEnemy dt gameState@(GameState {ticker=currentTick,background=background}) enemy = newEnemy
  where
    -- Update the last fired tick
    canFire = (currentTick - (lastFiredTick enemy)) > (wepCooldown enemy)
    updatedTick =
      case canFire of
        False -> lastFiredTick enemy
        True  -> currentTick
    enemyObj = shipObj enemy
    newEnemyObj = enemyObj
    -- Movement
    (dx,dy) = direction newEnemyObj
    enemySpeed = speed enemyObj
    deltaPos = (dx*enemySpeed*dt,dy*enemySpeed*dt)
    newEnemy = enemy { shipObj = (move deltaPos newEnemyObj), lastFiredTick = updatedTick, isFiring=(not $ outOfBounds newEnemyObj background) }
