-- https://www.codewars.com/kata/5259510fc76e59579e0009d4
module DidYouMean (findMostSimilar) where

import Data.List (minimumBy, zipWith4)
import Data.Ord (comparing)

findMostSimilar :: [String] -> String -> String
findMostSimilar dictionary term = minimumBy (comparing (distance term)) $ dictionary

distance :: String -> String -> Int
distance s = last . foldl (nextCol s) [0..length s]
  where 
    nextCol s' xs@(x:xs') c = cur_col 
      where 
        cur_col = (x + 1) : zipWith4 induction (map (c==) s') xs xs' cur_col
        induction is_equal prev_col tail_prev_col cur_col = minimum [cur_col + 1,
                                                                   tail_prev_col + 1,
                                                                    prev_col + if is_equal 
                                                                               then 0 
                                                                               else 1]
