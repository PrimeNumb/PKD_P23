module Rendering where
import Graphics.Gloss
import DataTypes

{- makeRectangle position width height
   Constructs a drawable rectangle based on the given position and width, height.
   PRE: 
   RETURNS: 
   EXAMPLES: 
-}
makeRectangle :: (Float, Float) -> Float -> Float -> Picture
makeRectangle point@(x, y) width height = polygon [upperLeft, upperRight, lowerRight, lowerLeft]
  where
    upperLeft = (x-(width/2), y+(height/2))
    upperRight = (x+(width/2), y+(height/2))
    lowerRight = (x+(width/2), y-(height/2))
    lowerLeft = (x-(width/2), y-(height/2))

{- makeDrawable
Converts a game object into a picture ready to be drawn on the screen.
   PRE: 
   RETURNS: 
   EXAMPLES: 
-}
--makeDrawable :: Object -> Picture
--makeDrawable (Object {position = pos, graphic=g}) = uncurry translate pos $ g
