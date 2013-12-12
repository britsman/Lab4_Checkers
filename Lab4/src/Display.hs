module Display (display, centerPixels, renderNextMoves) where

import Graphics.UI.GLUT
import Control.Monad
import Data.IORef
import MyQuads
import Data.Graph.Wrapper 
import Players
import GameBoard

 
--display :: IORef GLfloat -> DisplayCallback
display :: IORef GLfloat -> IORef (Graph Int (Maybe Int)) -> IORef [Int] -> IORef Int -> DisplayCallback
display selPos gameboard activePlayers indexCurrentPlayer = do
  gb <- get gameboard
  clear [ColorBuffer]
  forM_ centerPixels $ \(x,y) -> myQuads (x,y) colorBlue   -- draw nodes
  actPlayers <- get activePlayers
  forM_ actPlayers $ \x -> renderMarbles x $ (playerCurrent x gb) 
  currentlySelected <- get selPos
  renderSelected' (round currentlySelected) colorWhite -- draw node that is selected
  -- draw possible nodes it can move to IF a node is selected
  renderNextMoves (round currentlySelected) gb
  swapBuffers
 
renderNextMoves :: Int -> Graph Int (Maybe Int) -> IO ()
renderNextMoves curSel gb 
              | curSel /= 0 = forM_ nextMoveInds $ \x -> myQuads 
                      ((!!) centerPixels (x-1)) colorNextMoves
              | otherwise     = return()
                  where
                    nextMoveInds = posNextMoves curSel gb
                    
renderMarbles :: Int -> [Int] -> IO ()
renderMarbles iPlayer indsMarbles 
             | indsMarbles /= [] = forM_ indsMarbles $ \x -> myQuads 
                      ((!!) centerPixels (x-1)) $ (!!) colorPlayers $ iPlayer - 1 
             | otherwise         = return()        

--renderMarbles :: Int -> [Int] -> IO ()
--renderMarbles iPlayer indsMarbles = forM_ indsMarbles $ \x -> myQuads 
--                      ((!!) centerPixels (x-1)) $ (!!) colorPlayers $ iPlayer - 1 
 
 
   
colorBlue :: (GLfloat,GLfloat,GLfloat) 
colorBlue = (0.0,0.0,1.0)

colorWhite :: (GLfloat,GLfloat,GLfloat) 
colorWhite = (1.0,1.0,1.0)

colorNextMoves :: (GLfloat,GLfloat,GLfloat) 
colorNextMoves = (1.0,0.5,0.5)

colorPlayers :: [(GLfloat,GLfloat,GLfloat)]
colorPlayers = [(1.0, 0.0, 0.0),(0.5 , 0.5 , 0.0),(0.0 , 0.5 , 0.0) ,
                 (0.5 , 0.0 , 0.0),(0.5, 1.0, 0.0), (0.0, 1.0, 0.0)]

centerPixels :: [(GLfloat, GLfloat)] 
centerPixels = pixelLocator [1,2,3,4,13,12,11,10,9,10,11,12,13,4,3,2,1] 0


pixelLocator :: [Int] -> Int -> [(GLfloat, GLfloat)]                                      
pixelLocator []     _ = []
pixelLocator (x:xs) n = zip xPixels yPixels ++ pixelLocator xs (n + 1) 
             where 
                 xPixels = replicate x (realToFrac (22 + 41 * n) :: GLfloat)
                 yPixels = [realToFrac (350 + 21 * x - 41*ind) :: GLfloat | ind <- [1..x]] 
                     


renderSelected' :: Int -> (GLfloat,GLfloat,GLfloat) -> IO()
renderSelected' curSel (r,g,b) 
                     | curSel /= 0 = do  
                         myQuads ((!!) centerPixels $ curSel - 1 ) (r,g,b) 
                         putStrLn $ "currently selected is " ++ show curSel
                     | otherwise = return()

