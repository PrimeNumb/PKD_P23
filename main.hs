module Main where
import Graphics.Gloss

main = do
  let window = (InWindow "Sample Window" (500, 500) (10, 10))
      sampleCircle = (circle 69)
  display window white sampleCircle
