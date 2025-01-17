module Encounter where
import DataTypes
import System.Random
import Presets
import Utilities

{- pop list1 list2
   Takes the head of a list and prepends it to another list.
   PRE: True
   RETURNS: A 2-tuple cotaining two lists, where the first element of the tuple is the tail of list1
   and the second element of the tuple is the head of list1 prepended to list2.
   EXAMPLES: pop [1,2] [3,4] == ([2], [1,3,4])
-}
pop :: [a] -> [a] -> ([a],[a])
pop [] container = ([],container)
pop (x:xs) container = (xs, x:container)


{- push list1 elem
   Prepends an element to a list.
   PRE: True
   RETURNS: A list with elem prepended to list1. 
   EXAMPLES: push [1,2] 3 == [3,1,2]
-}
push :: [a] -> a -> [a]
push stack element = element:stack


{- shouldPopEncounter tick encounter
   Checks if a ship should be loaded into the game.
   PRE: True
   RETURNS: True if the interval between tick and the last time a ship was loaded is equal to/greater than the minimum time required between loading ships,
   otherwise False.
   EXAMPLES: shouldPopEncounter 15.0 defaultEncounter == True
-}
shouldPopEncounter :: Float -> Encounter -> Bool
shouldPopEncounter currentTick (Encounter {popInterval=popInterval,lastPop=lastPop}) =
  (currentTick - lastPop) >= popInterval

{- generateShips randomGen nrOfShips shipTemplate
   Generates a set of ships and a new random generator.
   PRE: nrOfShips >= 0
   RETURNS: A 2-tuple where the first element is a list of generated ships based on nrOfShips and shipTemplate,
   and the second element is a new random generator based on randomGen.
   If nrOfShips == 0 then the first element is simply the empty list.
   EXAMPLES: generateShips (mkStdGen 123) 0 playerShipDefault == ([], 124 1)
-}
generateShips :: StdGen -> Int -> Ship -> ([Ship], StdGen)
generateShips gen nrOfShips template = (ships, newGen)
  where
    (ships, newGen) = generateShipsAux gen nrOfShips template []

{- generateShipsAux randomGen nrOfShips shipTemplate acc
   Generates a set of ships and a new random generator.
   PRE: nrOfShips >= 0
   RETURNS: A 2-tuple where the first element is a list of generated ships
   (based on nrOfShips & shipTemplate) prepended to acc, and the second element is a new random generator based on randomGen.
   EXAMPLES: generateShipsAux (mkStdGen 123) 0 playerShipDefault [] == ([], 124 1)
-}
generateShipsAux :: StdGen -> Int -> Ship -> [Ship] -> ([Ship], StdGen)
generateShipsAux gen 0 template acc = (acc, gen)
generateShipsAux gen nrOfShips template acc =
  generateShipsAux newGen (nrOfShips-1) template (newShip:acc)
  where
    (newShip, newGen) = generateShip gen template


{- generateShip randomGen shipTemplate
   Generates a ship and a new random generator.
   PRE: True
   RETURNS: A 2-tuple where the first element is a new ship based on randomGen and shipTemplate,
   and the second element is a new random generator based on randomGen.
   EXAMPLES: generateShip (mkStdGen 123) playerShipDefault == (Ship {...}, 858493321 1872071452)
-}
generateShip :: StdGen -> Ship -> (Ship, StdGen)
generateShip gen shipTemplate = (shipTemplate {shipObj=setPos (xPos,yPos) (shipObj shipTemplate),wepCooldown=cooldown}, gen2)
  where
    shipWidth = fst $ bounds $ shipObj shipTemplate
    shipHeight = snd $ bounds $ shipObj shipTemplate
    xPos = winWidth/2 + shipWidth
    (yPos, gen1) = randomR ((-winHeight+shipHeight)/2,(winHeight-shipHeight)/2) gen :: (Float, StdGen)
    (cooldown, gen2) = randomR (1.5, 2.0) gen1 :: (Float, StdGen)

{-updateEncounter encounter currentTick enemyContainer
  Checks if an enemy should spawn from an Encounter stack by looking at the current tick. If that is the case it is moved into a list of ships.
  PRE: True
  RETURNS: A 2-tuple of the updated encounter.
  EXAMPLES:
-}
updateEncounter :: Encounter -> Float -> [Ship] -> (Encounter,[Ship])
updateEncounter encounter currentTick enemyContainer
  | shouldPopEncounter currentTick encounter = (newEncounter, newEnemyContainer)
  | otherwise = (encounter, enemyContainer)
  where
    updatedEncounter = encounter {lastPop=currentTick}
    (newStack, newEnemyContainer) =
      pop (shipStack updatedEncounter) enemyContainer
    newEncounter = updatedEncounter {shipStack=newStack}
