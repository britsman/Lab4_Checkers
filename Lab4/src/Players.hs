module Players (initPlayers, playerWon, prop_player_hasNotWon, playerCurrent) where
import GameBoard
import Data.Graph.Wrapper
import Test.QuickCheck

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

-- Gets the positions of a certain player's marbles.                 
--playerCurrent :: Int -> Graph Int (Maybe Int) -> [Int]
--playerCurrent p g = find p 1 0
--          where
--              find p' i n    
--                      | n == 10 = [] 
--                      | vertex g i == Just p' = i : find p' (i+1) (n+1)
--                      | otherwise = find p' (i+1) n 
                      
                      
    
playerCurrent :: Int -> Graph Int (Maybe Int) -> [Int]
playerCurrent p g = find 1 0
          where
              find i n    
                      | n == 10 = [] 
                      | vertex g i == Just p = i : find (i+1) (n+1)
                      | otherwise = find (i+1) n
                      
                      
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
             
{- Fills up the winning destination of the chosen player, but NOT with the
   values needed to win. -}                            
prop_player_hasNotWon :: TestingGraph -> Property
prop_player_hasNotWon g = forAll (choose (1,6)) (\x -> case even x of
                         False -> not $ playerWon (x+1) $ initPlayers [x] g'
                         _ -> not $ playerWon (x-1) $ initPlayers [x] g')
                      where g' = graph g