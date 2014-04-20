module Calc where
 
import Data.Graph.Wrapper

vertex' :: Int -> Graph Int (Maybe Int) -> Maybe Int
vertex' i g = vertex g i