module Bindings (display, reshape, keyboardMouse) where
import Data.IORef
import Graphics.UI.GLUT
import Display
 
reshape :: ReshapeCallback
reshape (Size 900 700) = windowSize $= Size 900 700
reshape _ = reshape (Size 900 700)
 
keyboardMouse :: KeyboardMouseCallback
keyboardMouse key Down _ pos = case key of
  (MouseButton LeftButton) -> putStrLn (show pos)
  _ -> return ()
keyboardMouse _ _ _ _ = return ()