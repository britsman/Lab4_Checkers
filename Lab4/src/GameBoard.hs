module GameBoard (createBoard, update, possibleDestinations,  TestingGraph,
graph, prop_update_success, prop_isAdjacent, prop_blank_destinations,
saveBoard, getSaves, loadBoard, deleteSaves, prop_save_load, posNextMoves,makeMove) where
import Data.Graph.Wrapper
import Data.Maybe
import Data.List (nub, partition, (\\))
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Data.Char

-- Returns the intended lengths for the middle rows.
rowLengthsMid :: [Int]
rowLengthsMid = [13,12..9]

{- Returns the intended lengths of the rows within the triangle, as well as 
   the length of the middle row that it connects to. -}
rowLengthsTri :: [Int]
rowLengthsTri = [4,3..1] 

-- Filepath to saves.
saveLocation :: FilePath
saveLocation = "../saves/saves.txt"

{- Creates the full gameboard by constructing a graph from the upper/lower
   half/triangle. Edges only need to be constructed in one direction, 
   since each edge contains both nodes (1 edge = 2 patterns to match on). -}              
createBoard :: Graph Int (Maybe Int)
createBoard = fromList $ map (\(x,y) -> (x,Nothing,y)) 
                             (concat (createMid 11 rowLengthsMid (+) ++ 
                             createMid 111 rowLengthsMid (-) ++ 
                             createTri 10 (-) ++ createTri 112 (+)) ++ cr)
               where cr = (57, [58]): [(x::Int, [x-1]) | x <- [58..65]]

{- Creates the upper/lower half of the board (without the triangle), 
   depending on which operator is passed in. -}
createMid :: Int -> [Int] -> (Int -> Int -> Int) -> [[(Int, [Int])]]             
createMid _ (_:[]) _ = [] 
createMid n (r1:(r2:rs)) op = createRow 0 n r1 r2 op 
                              : createMid (n `op` r1) (r2:rs) op
         where
             createRow _ _ 0 _ _ = []
             createRow ns n' r1' r2' op' = (n',e) 
                      : createRow (ns+1) (n' `op'` 1) (r1'-1) r2' op'
                        where 
                            e = e1 ++ e2
                            e1 = if ns > 0 then [n' `op'` (-1),n' `op'` r2'] 
                                 else []
                            e2 = if r1' /= 1 then [n' `op'` r2' `op` 1] 
                                 else []

{- Creates the upper/lower triangle part of the board, depending on which
   operator is passed in. -}
createTri :: Int -> (Int -> Int -> Int) -> [[(Int, [Int])]] 
createTri n op = [(n',[n' `op` (-1)])]
                 : connectTriMid (createMid n  rowLengthsTri op) 
            where
                n' = n `op` 9
                connectTriMid (r:rs) = connect' r : rs
                connect' [] = []
                connect' ((i,es):is) = (i, es' ++ es) : connect' is
                      where
                          es' = i `op` (-8):[i `op` (-9)]

-- Updates the node value at a certain index in the graph.                          
update :: Int -> Maybe Int -> Graph Int (Maybe Int) -> Graph Int (Maybe Int)
update i v g = fromList $ a ++ b'  
           where
               (a,b) = splitAt (i-1) (toList g)
               b' = if null b then b else change (head b) : tail b
               change (i', _, es) =  (i', v, es)

-- Updates moved from node to nothing and movet to node to player value.               
makeMove :: Int -> Int -> Graph Int (Maybe Int) -> Graph Int (Maybe Int)
makeMove fromNode toNode gb = update toNode val tempGB
             where val = vertex gb fromNode
                   tempGB = update fromNode Nothing gb
                    

-- Returns the indexes of all nodes adjacent to the node it is pointed to.               
getAdjacent :: Int -> Graph Int (Maybe Int) ->  [Int]
getAdjacent i g = successors g i ++ successors (transpose g) i

{- Messy pathcalcualtion, works and is fast but need to clean up code so it 
   looks better, use less variables etc. -}
possibleDestinations :: [Int] -> [Int] -> Graph Int (Maybe Int) -> [Int]
possibleDestinations [] ds _ = ds
possibleDestinations (i:is) ds g = possibleDestinations is2 ds2 g 
        where 
            aj = (getAdjacent i g)
            ajs = l' \\ (l \\ l')
            l = aj ++ (concatMap (\x -> getAdjacent x g) aj)
            l' = nub l
            a' = if null ds then a else []
            (a,b) = partition (\x -> isNothing (vertex g x)) (filter (\x-> x `notElem` ds) aj)
            (is2, ds2) = tryPath b is (a' ++ ds)
            tryPath [] ps' ds' = (ps', ds')
            tryPath (i':is') ps' ds' = 
               case filter (\x-> x `elem` ajs && 
                             x `notElem` ds') (getAdjacent i' g) of
                  [] -> tryPath is' ps' ds'
                  (x:_) -> case vertex g x of
                              Nothing -> tryPath is' (x:ps') (x:ds') 
                              _ -> tryPath is' ps' ds'

-- Used to call possibleDestinations from GUI
posNextMoves :: Int -> Graph Int (Maybe Int) -> [Int]
posNextMoves i g = possibleDestinations [i] [] g 

test i g = (ajs, l, l', aj)
         where 
            aj = (getAdjacent i g)
            ajs = l' \\ (l \\ l')
            l = aj ++ (concatMap (\x -> getAdjacent x g) aj)
            l' = nub l

-- Saves remaining players and state of board to file.
saveBoard :: [Int] -> Graph Int (Maybe Int) -> IO ()
saveBoard ps g =  do
                   appendFile saveLocation gameState
                    where 
                        gameState =  (concatMap show ps) ++ 
                                     show (marbles (toList g)) ++ "\n"
                        marbles [] = []
                        marbles ((i, v, _):is) = (i,v) : marbles is

-- Gets available saves so user can choose one to load.
getSaves :: IO [([Int], String)]
getSaves = do
            f <- readFile saveLocation
            return (map saves (lines f))
        where
            saves s = (map digitToInt (takeWhile (/= '[') s), 
                      (dropWhile (/= '[') s))

-- Deletes saves by overwriting the file with the saves to be kept.                    
deleteSaves :: [([Int], String)] -> IO ()
deleteSaves [([],"")] = writeFile saveLocation "" 
deleteSaves sl = writeFile saveLocation sl' 
            where 
               sl' = concatMap (\(x,y) -> map intToDigit x ++ y ++ "\n") sl  
                          
-- Loads the chosen save.
loadBoard :: String -> Graph Int (Maybe Int)
loadBoard s = load b (createBoard)
          where
              b = read (s)::[(Int, Maybe Int)]
              load [] g = g
              load ((i,v):is) g = load is (update i v g)

{- Verifies that the call to update correctly changed node value at the
   correct index. -}                             
prop_update_success :: TestingGraph -> Property
prop_update_success g = forAll (choose (1,121)) (\x -> Just 1 == 
                               vertex (update x (Just 1) g') x)
                         where g' = graph g
                        
-- Checks that all nodes x has adjacency to, have x as an adjacency as well.                            
prop_isAdjacent :: TestingGraph -> Property
prop_isAdjacent g = forAll (choose (1,121)) 
                           (\x -> all (\y -> x `elem` getAdjacent y g') 
                           $ getAdjacent x g')
                     where g' = graph g

{- Checks that all possible destinations given for a certain index actually
   are empty. -}                              
prop_blank_destinations :: TestingGraph -> Property
prop_blank_destinations g = forAll (choose (1,121)) (\x -> all 
                                   (\y -> isNothing (vertex g' y)) 
                                   $ possibleDestinations [x] [] g')
                             where g' = graph g

{- Verifies that a Graph is still the same after it's been saved and loaded.
   It is recommended to run deleteSaves [([], "")] after testing to 
   clean up the file. -}                             
prop_save_load :: TestingGraph -> Property
prop_save_load g =  monadicIO $ do
                     run (saveBoard [1..6] g1)
                     sl <- run (getSaves)
                     (_, s) <- pick (return (last sl))
                     g2 <- pick (return (loadBoard s))
                     assert (toList g2 == toList g1)
               where 
                   g1 = graph g
                       
{- Abstraction of graph-wrapper library Graph datatype (used to generate
   arbitrary Graphs). -}
data TestingGraph = TestingGraph { graph :: Graph Int (Maybe Int) }
                deriving Show

{- Arbitrary gameboards for QuickCheck testing. These boards are not meant 
   to be used in the game, they are only for verifying correctness of the
   backend code. -}
instance Arbitrary TestingGraph where
 arbitrary = do
               g <- genTestGraph 1 createBoard
               return (TestingGraph g)

{- Used to generate randomly filled gameboards for testing backend logic.
   varied boards are prioritized over test execution time (so runnning the
   properties will take a few seconds -}                          
genTestGraph :: Int -> Graph Int (Maybe Int) -> Gen (Graph Int (Maybe Int))
genTestGraph 122 g = return g
genTestGraph i g = do
             v <- elements (Nothing : [Just x | x <- [1..6]])
             genTestGraph (i+1) (update i v g)
