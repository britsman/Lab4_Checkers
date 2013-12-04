module Players (initPlayers, playerWon) where
import GameBoard
import Data.Graph.Wrapper

{- Returns the indexes that need to be updated in order to allow a certain
   player to participate. -}
playerStart :: Int -> [Int]
playerStart n = 
            case n of
                 1 -> [1..10]
                 2 -> [112..121]
                 3 -> [11..14] ++ [24..26] ++ [36,37] ++ [47]
                 4 -> 75 : [85,86] ++ [96..98] ++ [108..111] 
                 5 -> [20..23] ++ [33..35] ++ [45,46] ++ [56]
                 6 -> 66 : [76,77] ++ [87..89] ++ [99..102] 
                 _ -> []

{- Fills the gameboard with initial values depending on how many players
   are participating (1-6) -}
initPlayers :: [Int] -> Graph Int (Maybe Int) -> Graph Int (Maybe Int) 
initPlayers [] g = g
initPlayers (p:ps) g = initPlayers ps (fill (playerStart p) g)
            where 
                fill [] g' = g'
                fill (i:is) g' = fill is (update i (Just p) g')
           
{- Given an Int ranging from 1-6, checks if that player has filled out the
   opposite starting pos of his/her own (which is the winning condition). -}     
playerWon :: Int -> Graph Int (Maybe Int) -> Bool
playerWon p g
          | even p = check (playerStart (p-1))
          | otherwise = check (playerStart (p+1))
             where
                 check [] = True
                 check (i:is)
                            | vertex g i /= Just p = False
                            | otherwise = check is