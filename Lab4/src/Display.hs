module Display (display) where
import Graphics.UI.GLUT
import Control.Monad
import Data.IORef
import Calc

background :: [(GLfloat, GLfloat, GLfloat, GLfloat , GLfloat)]
background = [(-1,1,0,0,0), (1,1,0,1,0), (1,-1,0,1,1), (-1,-1,0,0,1)]
 
display :: TextureObject -> DisplayCallback
display t' = do 
  let color3f r g b = color $ Color3 r g (b :: GLfloat)
  clear [ColorBuffer, DepthBuffer] -- clear depth buffer, too
  clear [ColorBuffer]
  loadIdentity
  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  texture Texture2D $= Enabled
  textureBinding Texture2D $= Just t'
  let tx x' y' z' = texCoord $ TexCoord3 (x' :: GLfloat) (y' :: GLfloat) (z' :: GLfloat)
  renderPrimitive Polygon $ do
     color3f 1 1 1
     mapM_ (\(x, y, z,x',y') -> (tx x' y' z) >> (vertex $ Vertex3 x y z)) background
  swapBuffers
  textureBinding Texture2D $= Nothing
  texture Texture2D $= Disabled
  blend $= Disabled