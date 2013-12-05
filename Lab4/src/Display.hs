module Display (display) where
import Graphics.UI.GLUT
import Control.Monad
import Data.IORef
import Calc
import MyPolygon

background :: [(GLfloat, GLfloat, GLfloat, GLfloat , GLfloat)]
background = [(-1,1,0,0,0), (1,1,0,1,0), (1,-1,0,1,1), (-1,-1,0,0,1)]

 
 
display :: TextureObject -> IORef GLfloat -> DisplayCallback
display t' selectedZone = do 
  let color3f r g b = color $ Color3 r g (b :: GLfloat)
  let vertex3f x y z = vertex $ Vertex3 x y (z :: GLfloat)
  clear [ColorBuffer, DepthBuffer] -- clear depth buffer, too
  --clear [ColorBuffer]
  loadIdentity
  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  texture Texture2D $= Enabled
  textureBinding Texture2D $= Just t'
  let tx x' y' z' = texCoord $ TexCoord3 (x' :: GLfloat) (y' :: GLfloat) (z' :: GLfloat)
  renderPrimitive Polygon $ do
     color3f 1 1 1
     mapM_ (\(x, y, z,x',y') -> (tx x' y' z) >> (vertex $ Vertex3 x y z)) background
     
  forM_ points  $ \(x,y) ->
    preservingMatrix $ do
      let radius = 0.05 
      color $ Color3 x y (radius :: GLfloat)
      polygon x y radius 
  selectedZone' <- get selectedZone
  polygon (xOfZone selectedZone') (0.2 :: GLfloat) (0.1 :: GLfloat)
--  renderPrimitive Quads $ do
--    color3f 1 0 0
--    vertex3f 0.4 0.4 (-0.1)
--    vertex3f 0.4 0.2 (-0.1)
--    vertex3f 0.2 0.2 (-0.1)
--    vertex3f 0.2 0.4 (-0.1) 
--    
--    color3f 0 1 0
--    vertex3f (-0.4) (-0.4) (-0.1) 
--    vertex3f (-0.4) (-0.2) (-0.1) 
--    vertex3f (-0.2) (-0.2) (-0.1) 
--    vertex3f (-0.2) (-0.4) (-0.1)   
--    
--    color3f 0 0 1
--    vertex3f (-1.2) (-1.2) (-0.1) 
--    vertex3f (-0.8) (-1) (-0.1) 
--    vertex3f (-0.7) (-0.8) (-0.1) 
--    vertex3f (-1) (-0.8) (-0.1) 
  swapBuffers
  textureBinding Texture2D $= Nothing
  texture Texture2D $= Disabled
  blend $= Disabled
  
  
  
points :: [(GLfloat,GLfloat)]
points = [ (-1-1/k + 2*m/k, -0.2) | m <- [1..k] ]
    where k = 17
    
xOfZone :: GLfloat -> GLfloat
xOfZone m =  -1-1/k + 2*m/k
    where k = 17