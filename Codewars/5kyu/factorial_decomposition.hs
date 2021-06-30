-- https://www.codewars.com/kata/5a045fee46d843effa000070
module FactorialDecomposition.Kata (decomp) where
import Data.List (intercalate)

primes = 2 : filter (null . tail . factors) [3, 5..]

factors :: Int -> [Int]
factors n = primeFactors n primes
  where
    primeFactors n ps@(p:ps')
      | p * p > n = [n]
      | n `mod` p == 0 = p : primeFactors (n `div` p) ps
      | otherwise = primeFactors n ps'

decomp :: Int -> String
decomp n = intercalate " * " $ zipWith printout <*> map (getExponents n) $ takeWhile (<=n) primes
  where 
    printout x y 
      | y /= 1 = show x ++ "^" ++ show y
      | otherwise = show x

getExponents :: Int -> Int -> Int
getExponents n p = sum . takeWhile (/=0) . map (div n) $ map (p^) [1..]
