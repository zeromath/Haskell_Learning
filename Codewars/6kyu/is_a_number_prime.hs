-- https://www.codewars.com/kata/5262119038c0985a5b00029f
module IsPrime where

primes = [2] ++ filter isPrime [3, 5..]

isPrime :: Integer -> Bool
isPrime x
  | x <= 1              = False
  | otherwise           = null . tail $ factors x primes
    where factors x (p:ps)
           | p * p > x      = [x]
           | x `mod` p == 0 = [p] ++ factors (x `div` p) (p:ps)
           | otherwise      = factors x ps
