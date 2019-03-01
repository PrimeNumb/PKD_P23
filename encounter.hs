module Encounter where
import Graphics.Gloss
import DataTypes
import Enemies
import System.Random
import Globals

-- Pops the first element from the stack and puts it into a container
--popEncounter :: Encounter -> [Ship] -> (Encounter,[Ship])
--popEncounter encounter@(Encounter {shipStack=[]}) container = (encounter,container)
--popEncounter encounter@(Encounter {shipStack=(x:xs)}) container =
--  ((encounter {shipStack=xs}),x:container)

pop :: [a] -> [a] -> ([a],[a])
pop [] container = ([],container)
pop (x:xs) container = (xs, x:container)

push :: [a] -> a -> [a]
push stack element = element:stack

--pushEncounter :: Encounter -> Ship -> Encounter
--pushEncounter encounter@(Encounter {shipStack=shipStack}) ship =
--  encounter {shipStack=(ship:shipStack)}

shouldPopEncounter :: Float -> Encounter -> Bool
shouldPopEncounter currentTick (Encounter {popInterval=popInterval,lastPop=lastPop}) =
  (currentTick - lastPop) >= popInterval

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
generateEnemyShip gen shipTemplate = (setPos (xPos,yPos) $ shipTemplate {wepCooldown=cooldown}, gen2)
  where
    enemyWidth = fst $ bounds $ shipObj shipTemplate
    xPos = winWidth/2 + enemyWidth
    (yPos, gen1) = randomR (-winHeight/2,winHeight/2) gen :: (Float, StdGen)
    (cooldown, gen2) = randomR (1.5, 2.0) gen1 :: (Float, StdGen)
