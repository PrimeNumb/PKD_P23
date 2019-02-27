module Encounter where
import Graphics.Gloss
import DataTypes
import Enemies
import System.Random
import Globals

-- Pops the first element from the stack and puts it into a container
popEncounterStack :: EncounterStack -> [Ship] -> (EncounterStack,[Ship])
popEncounterStack stack@(EncounterStack {ship_stack=[]}) container = (stack,container)
popEncounterStack stack@(EncounterStack {ship_stack=(x:xs)}) container =
  ((stack {ship_stack=xs}),x:container)
    

shouldPopEncounterStack :: Float -> EncounterStack -> Bool
shouldPopEncounterStack currentTick (EncounterStack {pop_interval=pop_interval,last_pop=last_pop}) =
  (currentTick - last_pop) >= pop_interval

-- WORK IN PROGRESS
generateEncounter :: Int -> [Ship]
generateEncounter nrOfShips = generateEncounterAux nrOfShips []

generateEncounterAux :: Int -> [Ship] -> [Ship]
generateEncounterAux 0 acc = acc
generateEncounterAux nrOfShips acc = generateEncounterAux (nrOfShips-1) acc
    
generateEnemyShip = do
  let pos_x = win_width/2 + enemy_width
  pos_y <- getStdRandom (randomR (-win_height/2,win_height/2)) :: IO Float
  cooldown <- getStdRandom (randomR (2.0, 4.0)) :: IO Float
  return $ setPos (pos_x,pos_y) enemyShipTemplate
  where
    enemy_width = fst $ boundingBox $ ship_obj enemyShipTemplate
