module Rendering where
import Graphics.Gloss
import Graphics.Gloss.Juicy
import DataTypes
import Globals

{- makeDrawable obj
   Converts a game object into a picture ready to be drawn on the screen.
   PRE: True
   RETURNS: A drawable picture based on the graphic of obj and the position of obj.
   EXAMPLES: makeDrawable playerDefaultObj == translate 0 0 $ color green $ rectangleSolid 50 50
-}
makeDrawable :: Object -> Picture
makeDrawable (Object {position = pos, graphic=g}) = uncurry translate pos $ g

{- drawWithBounds obj
   Convert a game object and its bounds to a picture ready to be drawn.
   PRE: True
   RETURNS: A picture based on the graphic and bounds of obj.
   EXAMPLES: drawWithBounds playerDefaultObj == pictures [(makeDrawable playerDefaultObj),(drawBounds playerDefaultObj)]
-}
drawWithBounds :: Object -> Picture
drawWithBounds obj@(Object {position=(x,y), bounds=(bx,by)}) =
    pictures $ (makeDrawable obj):(drawBounds obj):[]

{- drawBounds obj
   Constructs a drawable picture out of the bounds of an object.
   PRE: True
   RETURNS: A picture based on the bounds of obj.
   EXAMPLES: drawBounds (Object (0,0) (0,0) 300.0 (10,10) Blank) == (color red $ translate 0 0 $ rectangleWire 20 20)
-}
drawBounds :: Object -> Picture
drawBounds (Object {position=(x,y), bounds=(bx,by)}) =
    color red $ translate x y $ rectangleWire (2*bx) (2*by)

{- processSprite pic
   Processes a potential picture.
   PRE: True
   RETURNS: A picture if pic is a Just picture. Otherwise a placeholder graphic.
   EXAMPLES: processSprite (Just $ circleSolid 5) == circleSolid 5
-}
processSprite :: Maybe Picture -> Picture
processSprite Nothing = color green $ rectangleSolid 50 50
processSprite (Just pic) = pic


{- refreshGfx gameState
  Refreshes the graphics of game objects in a given game state.
  PRE: True
  RETURNS: A game state based on gameState where any object that is drawn has its default graphic updated to a graphic saved in gameState.
  EXAMPLES: refreshGfx defaultGameState
-}
refreshGfx :: Game -> Game
refreshGfx gameState@(GameState {gameGfx=gameGfx}) = newGameState
  where
    newPlyProj =
      (projectile $ player gameState) {projObj = (projObj $ projectile $ player gameState) {graphic=(playerProjGfx gameGfx)}}
    newPlayerTemplate =
      (player gameState) {shipObj = (shipObj $ player gameState) {graphic=(playerGfx gameGfx)}, projectile = newPlyProj}
    newEnmyProj =
      (projectile $ enmyTemplate gameState) {projObj = (projObj $ projectile $ enmyTemplate gameState) {graphic=(enemyProjGfx gameGfx)} }
    newEnmyTemplate =
      (enmyTemplate gameState) {shipObj = ((shipObj $ enmyTemplate gameState){graphic=(enemyStandardGfx gameGfx)}), projectile = newEnmyProj }
    newGameState = gameState
      {
        player = newPlayerTemplate,
        plyTemplate = newPlayerTemplate,
        enmyTemplate = newEnmyTemplate,
        plyProjTemplate = (plyProjTemplate gameState) {projObj = (projObj $ plyProjTemplate gameState) {graphic=(playerProjGfx gameGfx)}},
        enmyProjTemplate = (enmyProjTemplate gameState) {projObj = (projObj $ enmyProjTemplate gameState) {graphic=(enemyProjGfx gameGfx)}},
        background = (background gameState) {graphic=(backgroundGfx gameGfx)}
      }


{- loadGfx
   Loads pictures from predestined filepaths.
   PRE: True
   RETURNS: A container containing the pictures if they loaded successfully. Any image that did not is replaced by a dummy image. 
   SIDE EFFECTS: IO; loading images. The exception handling if a filepath is invalid is handled in loadJuicyPNG. 
   EXAMPLES: loadGfx
-}
loadGfx :: IO GameGfx
loadGfx = do
  imgBuffer <- loadJuicyPNG playerSpritePath
  let playerGfx = processSprite imgBuffer
  
  imgBuffer <- loadJuicyPNG enemySpritePath
  let enemyStandardGfx = processSprite imgBuffer

  imgBuffer <- loadJuicyPNG plyProjSpritePath
  let playerProjGfx = processSprite imgBuffer

  imgBuffer <- loadJuicyPNG enemyProjSpritePath
  let enemyProjGfx = processSprite imgBuffer

  imgBuffer <- loadJuicyPNG heartSpritePath
  let heartGfx = processSprite imgBuffer

  imgBuffer <- loadJuicyPNG gameOverSpritePath
  let gameOverGfx = processSprite imgBuffer
  
  imgBuffer <- loadJuicyPNG backgroundPath
  let backgroundGfx = processSprite imgBuffer
  let gameGfx =
        defaultGameGfx
        {
          playerGfx = playerGfx,
          enemyStandardGfx = enemyStandardGfx,
          playerProjGfx = playerProjGfx,
          enemyProjGfx = enemyProjGfx,
          heartGfx = heartGfx,
          gameOverGfx = gameOverGfx,
          backgroundGfx = backgroundGfx
        }
  return gameGfx
