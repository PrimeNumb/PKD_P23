module Encounter where
import Graphics.Gloss
import DataTypes
import Enemies
import System.Random
import Globals

-- Pops the first element from the stack and puts it into a container
--popEncounter :: Encounter -> [Ship] -> (Encounter,[Ship])
--popEncounter encounter@(Encounter {ship_stack=[]}) container = (encounter,container)
--popEncounter encounter@(Encounter {ship_stack=(x:xs)}) container =
--  ((encounter {ship_stack=xs}),x:container)

pop :: [a] -> [a] -> ([a],[a])
pop [] container = ([],container)
pop (x:xs) container = (xs, x:container)

push :: [a] -> a -> [a]
push stack element = element:stack

--pushEncounter :: Encounter -> Ship -> Encounter
--pushEncounter encounter@(Encounter {ship_stack=ship_stack}) ship =
--  encounter {ship_stack=(ship:ship_stack)}

shouldPopEncounter :: Float -> Encounter -> Bool
shouldPopEncounter currentTick (Encounter {pop_interval=pop_interval,last_pop=last_pop}) =
  (currentTick - last_pop) >= pop_interval

-- WORK IN PROGRESS
generateEncounter :: StdGen -> Int -> Ship -> ([Ship], StdGen)
generateEncounter gen nrOfShips template = (ships, newGen)
  where
    (ships, newGen) = generateEncounterAux gen nrOfShips template []

generateEncounterAux :: StdGen -> Int -> Ship -> [Ship] -> ([Ship], StdGen)
generateEncounterAux gen 0 template acc = (acc, gen)
generateEncounterAux gen nrOfShips template acc =
  generateEncounterAux newGen (nrOfShips-1) template (newShip:acc)
  where
    (newShip, newGen) = generateEnemyShip gen template

generateEnemyShip :: StdGen -> Ship -> (Ship, StdGen)
generateEnemyShip gen shipTemplate = (setPos (pos_x,pos_y) $ shipTemplate {wep_cooldown=cooldown}, gen2)
  where
    enemy_width = fst $ bounds $ ship_obj shipTemplate
    pos_x = win_width/2 + enemy_width
    (pos_y, gen1) = randomR (-win_height/2,win_height/2) gen :: (Float, StdGen)
    (cooldown, gen2) = randomR (1.5, 2.0) gen1 :: (Float, StdGen)
