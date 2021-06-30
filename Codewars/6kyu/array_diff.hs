-- https://www.codewars.com/kata/523f5d21c841566fde000009
module Difference where

difference :: Eq a => [a] -> [a] -> [a]
difference a b = filter (\x -> not (x `elem` b)) a
