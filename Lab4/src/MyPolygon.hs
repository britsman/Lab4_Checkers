module MyPolygon where
 
import Graphics.UI.GLUT
 
vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 x y z
 
polygon :: GLfloat -> GLfloat -> GLfloat -> IO ()
polygon x y r = renderPrimitive Polygon $ mapM_ vertex3f 
   [ (x + r * sin ( 2 * pi * k / 6), y + r * cos (2 * pi * k / 6), z) | k <- [1..6] ]
      where z = -0.1
    

   