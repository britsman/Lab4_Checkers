module GameBoard (createBoard, update, possibleDestinations) where
import Data.Graph.Wrapper
import Data.Maybe
import Data.List (nub, partition, (\\))

-- Returns the intended lengths for the middle rows.
rowLengthsMid :: [Int]
rowLengthsMid = [13,12..9]

{- Returns the intended lengths of the rows within the triangle, as well as 
   the length of the middle row that it connects to. -}
rowLengthsTri :: [Int]
rowLengthsTri = [4,3..1] 

{- Creates the full gameboard by constructing a graph from the upper/lower
   half/triangle. Edges only need to be constructed in one direction, 
   since each edge contains both nodes (1 edge = 2 patterns to match on). -}              
createBoard :: Graph Int (Maybe Int)
createBoard = fromList $ map (\(x,y) -> (x,Nothing,y)) 
                             (concat ((createMid 11 rowLengthsMid (+)) ++ 
                             (createMid 111 rowLengthsMid (-)) ++ 
                             (createTri 10 (-)) ++ createTri 112 (+)) ++ cr)
               where cr = ((57, [58]): [(x::Int, [x-1]) | x <- [58..65]])

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

-- Returns the indexes of all nodes adjacent to the node it is pointed to.               
getAdjacent :: Int -> Graph Int (Maybe Int) ->  [Int]
getAdjacent i g = successors g i ++ successors (transpose g) i

{- Messy pathcalcualtion, works and is fast but need to clean up code so it 
   looks better, use less variables etc. -}
possibleDestinations :: [Int] -> [Int] -> Graph Int (Maybe Int) -> [Int]
possibleDestinations [] ds _ = ds
possibleDestinations (i:is) ds g = possibleDestinations is2 ds2 g 
        where 
            aj = (filter (\x-> x `notElem` ds) (getAdjacent i g))
            ajs = l' \\ (l \\ l')
            l = concatMap (\x -> getAdjacent x g) aj
            l' = nub l          
            a' = if null ds then a else []
            (a,b) = partition (\x -> isNothing (vertex g x)) aj
            (is2, ds2) = tryPath b is (a' ++ ds)
            tryPath [] ps' ds' = (ps', ds')
            tryPath (i':is') ps' ds' = 
               case filter (\x-> x `elem` ajs && 
                             x `notElem` ds') (getAdjacent i' g) of
                  [] -> tryPath is' ps' ds'
                  (x:_) -> case vertex g x of
                              Nothing -> tryPath is' (x:ps') (x:ds') 
                              _ -> tryPath is' ps' ds'           
