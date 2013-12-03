module GameBoard (createBoard) where
import Data.Graph.Wrapper

-- index = actual position of node. value = Nothing or player 1-6 marble.
type Node = (Int,Maybe Int)

-- Returns the intended lengths for the middle rows.
rowLengthsMid :: [Int]
rowLengthsMid = [13,12..9]

{- Returns the intended lengths of the rows within the triangle, as well as 
   the length of the middle row that it connects to. -}
rowLengthsTri :: [Int]
rowLengthsTri = [1..4] ++ [13]

{- Creates the full gameboard by constructing a graph from the upper/lower
   half/triangle. Edges only need to be constructed in one direction, 
   since each edge contains both nodes (1 edge = 2 patterns to match on). -}              
createBoard :: Graph Node Node
createBoard = fromListSimple $ concat ((createMid 11 rowLengthsMid (+)) ++ 
                       (createMid 111 rowLengthsMid (-))) ++ centerRow
               where centerRow = (((57, Nothing), [(58,Nothing)]): 
                                 [((x::Int,Nothing), [(x-1,Nothing)]) 
                                 | x <- [58..65]])

{- Creates the upper/lower half of the board (without the triangle), 
   depending on which operator is passed in. -}
createMid :: Int -> [Int] -> (Int -> Int -> Int) -> [[(Node, [Node])]]             
createMid _ (_:[]) _ = [] 
createMid n (r1:(r2:rs)) op = createRow 0 n r1 r2 op 
                              : createMid (n `op` r1) (r2:rs) op
         where
             createRow _ _ 0 _ _ = []
             createRow ns n' r1' r2' op' = 
                      ((n',Nothing),e) 
                      : createRow (ns+1) (n' `op'` 1) (r1'-1) r2' op'
                          where 
                              e = e1 ++ e2
                              e1 = if ns > 0 then [(n' `op'` (-1),Nothing), 
                                                  (n' `op'` r2',Nothing)] 
                                   else []
                              e2 = if r1' /= 1 then [(n' `op'` r2' `op` 1,
                                                    Nothing)] 
                                   else []

{- Creates the upper/lower triangle part of the board, depending on which
   operator is passed in. -}
--Not yet implemented