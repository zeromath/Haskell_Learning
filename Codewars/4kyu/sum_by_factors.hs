-- https://www.codewars.com/kata/54d496788776e49e6b00052f
module Codewars.Kata.SumByFactors where

import Data.List

primes = 2: filter (null . tail . primeFactors) [3, 5..]

primeFactors :: Integer -> [Integer]
primeFactors n 
  | n < 0     = primeFactors (-n)
  | otherwise = factors n primes
  where factors n (p: ps)
          | p * p > n      = [n]
          | n `mod` p == 0 = p: factors (n `div` p) (p:ps)
          | otherwise      = factors n ps

sumOfDivided :: [Integer] -> [(Integer, Integer)]
sumOfDivided xs = map (\y -> (y, sum . filter (\x -> x `mod` y == 0) $ xs)) all_prime_factors
  where all_prime_factors = sort . foldl1 union . map (nub . primeFactors) $ xs
