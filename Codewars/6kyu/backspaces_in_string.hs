-- https://www.codewars.com/kata/5727bb0fe81185ae62000ae3
module Codewars.BackspacesInString where

cleanString :: String -> String
cleanString = cleanUp [] 
  where 
    cleanUp xs cs
      | null cs        = xs
      | head cs == '#' = case null xs of 
                           True -> cleanUp xs (tail cs)
                           False -> cleanUp (init xs) (tail cs)
      | otherwise      = cleanUp (xs ++ [head cs]) (tail cs)
