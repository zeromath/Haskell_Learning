-- https://www.codewars.com/kata/521c2db8ddc89b9b7a0000c1
module Snail where

snail :: [[Int]] -> [Int]
snail xs = travel 0 xs
  where travel direction xs
          | xs == []       = []
          | direction == 0 = (head xs) ++ travel 1 (tail xs)
          | direction == 1 = (map last xs) ++ travel 2 (map init xs)
          | direction == 2 = (reverse $ last xs) ++ travel 3 (init xs)
          | direction == 3 = (reverse $ map head xs) ++ travel 0 (map tail xs)
