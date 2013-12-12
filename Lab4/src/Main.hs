module Main where

import Graphics.UI.GLUT
import Data.IORef
import Bindings
import GameBoard
import Display
import Players


main :: IO ()
main = main' 6
  
main' :: Int -> IO ()
main' nPlayers = do
  (_progName,  _args ) <- getArgsAndInitialize
  let playerList = [1..nPlayers]
  initialDisplayMode $= [DoubleBuffered]
  initialWindowSize $= Size 900 700
  initialWindowPosition $= Position 0 0
  _window <- createWindow "Hello test World" 
  reshapeCallback $= Just reshape
  selPos <- newIORef 0.0  
  currentPlayer <- newIORef 0
  gameboard <- newIORef $ initPlayers playerList createBoard
  activePlayers <- newIORef playerList
  keyboardMouseCallback $= Just (keyboardMouse selPos gameboard currentPlayer activePlayers)  
  displayCallback $= display selPos gameboard  activePlayers currentPlayer
  mainLoop
  
 