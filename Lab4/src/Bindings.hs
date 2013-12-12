module Bindings (reshape, keyboardMouse) where
 
import Graphics.UI.GLUT
import Data.IORef
import Display
import MyQuads
import Players
import Data.Graph.Wrapper
import Data.List ((\\))
import GameBoard 
 
reshape :: ReshapeCallback
reshape _ = windowSize $= Size 900 700

isPosInNode :: (GLfloat,GLfloat) -> (GLfloat,GLfloat) -> Bool  
isPosInNode (x',y') (x,y) = abs ( x - x') < radius && abs (y - y') < radius


keyboardMouse :: IORef GLfloat -> IORef (Graph Int (Maybe Int)) -> IORef Int  -> IORef [Int] -> KeyboardMouseCallback
keyboardMouse selPos gameboard indexCurrentPlayer activePlayers key Down _ pos = case key of

  (MouseButton LeftButton) -> do 
                print pos
                gb <- get gameboard
                indCurPlay <- get indexCurrentPlayer  
                fromNode <- get selPos
                actPlayers <- get activePlayers
                let curPlay = (!!) actPlayers indCurPlay
                let selectedNode = fromPosToIndex $ convertPosToFloat pos
                    nodesOfCurrentPlayer = playerCurrent curPlay gb 
                let clickedPositionIsOwnMarble = elem (round selectedNode) nodesOfCurrentPlayer
                    isMarBleSelected = fromNode /= 0                
                putStrLn $ "clicked on own marble " ++ show clickedPositionIsOwnMarble
                if clickedPositionIsOwnMarble
                      then writeIORef selPos selectedNode
                      else if isMarBleSelected
                               then  do 
                                        let isClickedNodePossibleDestination = elem (round selectedNode)  (posNextMoves (round fromNode) gb) -- curplay -> round fromnode
                                        putStrLn $ " selected move from" ++ show fromNode ++ " to " ++ 
                                           show selectedNode ++ " is " ++ show isClickedNodePossibleDestination
                                        if  isClickedNodePossibleDestination
                                            then do -- this block says what happens at end of move
                                                  writeIORef gameboard $ makeMove (round fromNode) (round selectedNode) gb
                                                  writeIORef selPos 0.0                                                                          
                                                  newGB <- get gameboard
                                                  if playerWon curPlay newGB && length actPlayers > 1
                                                     then writeIORef activePlayers (actPlayers \\ [curPlay]) 
                                                     else writeIORef activePlayers  actPlayers  -- = do nothing
                                                  if indCurPlay >= (length actPlayers - 1)
                                                     then writeIORef indexCurrentPlayer 0 
                                                     else writeIORef indexCurrentPlayer (indCurPlay + 1)
                                            else writeIORef selPos 0.0
                               else writeIORef selPos 0.0
                readIORef selPos >>= print
                postRedisplay Nothing
  (SpecialKey KeyLeft ) -> do
                putStrLn "left"
                selPos $~! \x -> max 0 $ x - 1  
                readIORef selPos >>= print
                postRedisplay Nothing
  (SpecialKey KeyRight) ->  do
                let nAvailPos = fromIntegral $ length centerPixels
                putStrLn "right"
                selPos $~! \x -> min nAvailPos $ x + 1  
                readIORef selPos >>= print
                postRedisplay Nothing
  (SpecialKey KeyF5) ->  do
                g <- get gameboard
                p <- get indexCurrentPlayer
                ps <- get activePlayers
                saveBoard (p:ps) g
  (SpecialKey KeyF7) ->  do
                writeIORef selPos 0.0
                sl <- getSaves
                case sl of
                 [] -> do return ()
                 sl' -> case last sl' of
                      ((x:xs), s) -> do
                         writeIORef indexCurrentPlayer x 
                         writeIORef activePlayers xs
                         writeIORef gameboard ((loadBoard s))
                postRedisplay Nothing  
  _ -> return ()
keyboardMouse _ _ _ _ _ _ _ _ = return ()
    

convertPosToFloat :: Position -> (GLfloat,GLfloat)
convertPosToFloat (Position x y) = (xf,yf) 
       where 
           xf = realToFrac x::GLfloat 
           yf = realToFrac y::GLfloat
    
fromPosToIndex :: (GLfloat,GLfloat) -> GLfloat      
fromPosToIndex pos = if null $ hits pos then 0.0 else head $ hits pos
         where hits pos' = [realToFrac ind :: GLfloat | ind <- [1..(length centerPixels)],
                   isPosInNode pos' $ (!!) centerPixels $ ind - 1]
