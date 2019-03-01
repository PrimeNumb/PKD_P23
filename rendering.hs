module Rendering where
import Graphics.Gloss
import DataTypes

{- makeRectangle position width height
   Constructs a drawable rectangle based on the given position and width, height
   PRE: True
   RETURNS: A rectangle with given width and height
-}
makeRectangle :: Position -> Float -> Float -> Picture
makeRectangle (x, y) width height = polygon [upperLeft, upperRight, lowerRight, lowerLeft]
  where
    upperLeft = (x-(width), y+(height))
    upperRight = (x+(width), y+(height))
    lowerRight = (x+(width), y-(height))
    lowerLeft = (x-(width), y-(height))

--makeRectangle' :: Position -> Float -> Float -> Picture
--makeRectangle' (x,y) width height

{- makeDrawable
Converts a game object into a picture ready to be drawn on the screen.
   PRE: 
   RETURNS: 
   EXAMPLES: 
-}
--makeDrawable :: Object -> Picture
--makeDrawable (Object {position = pos, graphic=g}) = uncurry translate pos $ g
