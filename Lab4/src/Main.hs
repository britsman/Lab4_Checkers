module Main where
import Graphics.UI.GLUT
import Graphics.GLUtil
import Data.IORef
import Bindings
import Display
 
main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
  initialWindowSize $= (Size 900 700)
  initialWindowPosition $= (Position 0 0)
  _window <- createWindow "Chinese Checkers"
  reshapeCallback $= Just reshape
  depthFunc $= Just Less
  keyboardMouseCallback $= Just (keyboardMouse)
  selectedZone <- newIORef (99.0 :: GLfloat)
  Right t <- readTexture "board_niceaspect.png"
  texture Texture2D $= Enabled
  textureBinding Texture2D $= Just t
  textureFilter Texture2D $= ((Linear', Nothing), Linear')
  textureWrapMode Texture2D S $= (Mirrored, ClampToEdge)
  textureWrapMode Texture2D T $= (Mirrored, ClampToEdge)
  textureBinding Texture2D $= Nothing
  texture Texture2D $= Disabled
  displayCallback $= display t selectedZone --angle pos
  mainLoop
