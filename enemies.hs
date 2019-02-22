module Enemies where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import Debug.Trace
import Rendering
import DataTypes

enemyObj1 :: Object
enemyObj1 = Object { position = (0, 0),
                     direction = (0, 0),
                     speed = 0,
                     boundingBox = ((0, 0), (0, 0)),
                     graphic = rectangleSolid 70.0 20.0
                   }
