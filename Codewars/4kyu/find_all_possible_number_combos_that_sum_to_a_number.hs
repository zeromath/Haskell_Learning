-- https://www.codewars.com/kata/555b1890a75b930e63000023
module Codewars.Kata.Combos where

allCombos = map combos [1..29]

combos :: Int -> [[Int]]
combos 1 = [[1]]
combos n = [n]:(concat . map boundedCombos $ [1..n - 1])
  where boundedCombos k = map (k:) . filter ((<=k) . maximum) $ allCombos !! (n - k - 1)
