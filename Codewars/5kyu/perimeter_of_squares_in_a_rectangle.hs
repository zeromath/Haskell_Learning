-- https://www.codewars.com/kata/559a28007caad2ac4e000083
module Codewars.Kata.Perimeter where

perimeter :: Integer -> Integer
perimeter = toInteger . flip (-) 4 . (*) 4 . (fibo !!) . (+) 3 . fromIntegral
  where
    fibo = 0 : 1 : (zipWith (+) <*> tail $ fibo)
