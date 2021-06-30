-- https://www.codewars.com/kata/55b7bb74a0256d4467000070
module ProperFractions.JorgeVS.Kata where

import Data.List (nub)

primes = 2 : filter (null .tail . myFactors) [3, 5..]

myFactors :: Integer -> [Integer]
myFactors n = getFactors n primes
  where 
    getFactors n ps'@(p:ps)
      | p * p > n = [n]
      | n `mod` p == 0 = p:getFactors (n `div` p) ps'
      | otherwise = getFactors n ps

properFractions :: Integer -> Integer
properFractions n = prod * (n `div` divi)
  where
    all_factors = nub . myFactors $ n
    divi = product all_factors
    prod = product $ map (subtract 1) all_factors
