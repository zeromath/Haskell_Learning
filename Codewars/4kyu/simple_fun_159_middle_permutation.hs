-- https://www.codewars.com/kata/58ad317d1541651a740000c5
module MiddlePermutation.JorgeVS.Kata where

import Data.List (sort)

middlePermutation :: String -> String
middlePermutation = middleSortedPermutation . sort
    
middleSortedPermutation :: String -> String
middleSortedPermutation xs
  | n <= 2 = xs
  | even n = (xs !! l) : reverse rem_str
  | otherwise = (xs !! l) : middleSortedPermutation rem_str
  where 
    n = length xs
    l = (n + 1) `div` 2 - 1
    rem_str = (take l xs) ++ drop (l+1) xs
