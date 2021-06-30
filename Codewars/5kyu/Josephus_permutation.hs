-- https://www.codewars.com/kata/5550d638a99ddb113e0000a2
module Josephus where

josephus :: [a] -> Int -> [a]
josephus [] _ = []
josephus xs k = reverse $ rotation k [] xs

rotation :: Int -> [a] -> [a] -> [a]
rotation _ xs [x] = x : xs
rotation k xs ys = rotation k ((ys !! (l-1)) : xs) (drop l ys ++ take (l-1) ys)
  where reduced_k = k `mod` length ys
        l = if reduced_k == 0 then min k $ length ys else reduced_k
