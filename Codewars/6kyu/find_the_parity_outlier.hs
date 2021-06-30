-- https://www.codewars.com/kata/5526fc09a1bbd946250002dc
module Kata (findOutlier) where

findOutlier :: [Int] -> Int 
findOutlier xs
  | length even_xs == 1 = head even_xs
  | otherwise           = head odd_xs
  where even_xs = filter even xs
        odd_xs  = filter odd xs
