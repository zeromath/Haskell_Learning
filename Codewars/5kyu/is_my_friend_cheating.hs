-- https://www.codewars.com/kata/5547cc7dcad755e480000004
module Codewars.Kata.RemovNB where
      
removNb :: Integer-> [(Integer, Integer)]
removNb n = map (\x -> (x, s `div` (x + 1) - 1)) a
  where a = filter (\x -> s `mod` (x + 1) == 0) [(n `div` 2 - 1)..n]
        s = 1 + n * (n + 1) `div` 2
