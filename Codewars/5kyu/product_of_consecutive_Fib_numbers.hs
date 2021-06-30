-- https://www.codewars.com/kata/5541f58a944b85ce6d00006a
module Codewars.Kata.Fib where

-- | Returns a pair of consecutive Fibonacci numbers a b,
--   where (a*b) is equal to the input, or proofs that the
--   number isn't a product of two consecutive Fibonacci 
--   numbers.
productFib :: Integer -> (Integer, Integer, Bool)
productFib n = (fibs !! l, fibs !! (l + 1), prod_fibs !! l == n)
  where fibs = scanl (+) 0 (1:fibs)
        prod_fibs = zipWith (*) fibs (tail fibs)
        l = length $ takeWhile (<n) prod_fibs
