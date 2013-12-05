module Bindings (display, reshape, keyboardMouse) where
import Data.IORef
import Graphics.UI.GLUT
import Display
 
reshape :: ReshapeCallback
reshape (Size 900 700) = windowSize $= Size 900 700 -- redundant?
reshape _ = reshape (Size 900 700)


 
keyboardMouse :: IORef GLfloat -> KeyboardMouseCallback 
keyboardMouse selectedZone (MouseButton LeftButton) Down _  =  do 
            putStrLn (show pos)
            updateSelectedZone pos
            return ()
  
keyboardMouse _ _ _ _ = return ()

updateSelectedZone :: Position -> GLfloat
updateSelectedZone pos = case pos of [ (-1 + 2*m/k, -1 + 2*(m+1)/k) | m <- [1..k] ]
    where k = 17