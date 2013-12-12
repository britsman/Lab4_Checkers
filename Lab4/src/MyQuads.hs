module MyQuads where
 
import Graphics.UI.GLUT

radius :: GLfloat
radius = 20.0
 
myQuads :: (GLfloat, GLfloat) -> (GLfloat, GLfloat, GLfloat) -> IO ()
myQuads (xPixCenter,yPixCenter) (r, g, b)= do  -- xPix,yPix in 0-700
  let color3f r' g' b' = color $ Color3 r' g' (b' :: GLfloat)
      pixToVertex x y = vertex $ Vertex3 (xConvPixToCoord x) (yConvPixToCoord y) (0 :: GLfloat)    
  renderPrimitive Quads $ do
    color3f r g b 
    pixToVertex (xPixCenter + radius) (yPixCenter + radius)
    pixToVertex (xPixCenter - radius) (yPixCenter + radius)
    pixToVertex (xPixCenter - radius) (yPixCenter - radius)
    pixToVertex (xPixCenter + radius) (yPixCenter - radius)
    
  
yConvPixToCoord :: GLfloat -> GLfloat
yConvPixToCoord y = (350-y)/350

xConvPixToCoord :: GLfloat -> GLfloat
xConvPixToCoord x = (x-450)/450


