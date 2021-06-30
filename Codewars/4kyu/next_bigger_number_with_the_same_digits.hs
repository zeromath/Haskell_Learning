-- https://www.codewars.com/kata/55983863da40caa2c900004e
module NextBigger (nextBigger) where

import Data.Char (digitToInt)
import Data.List (findIndex, sort)
import Data.Maybe (fromJust)

nextBigger :: Int -> Int
nextBigger n = if p == 0 then -1 else getNum $ (getNextLarger $ take p d) ++ (drop p d)
  where
    d = reverse $ digitToInt <$> show n
    p = 1 + getFirstDownPos 1 d
    getNum = sum . zipWith (*) ((10^) <$> [0..]) 
    
getNextLarger :: [Int] -> [Int]
getNextLarger xs' = reverse $ (((xs !! q) : take q xs) ++ (x : drop (q+1) xs))
  where 
    x = last xs'
    xs = init xs'
    q = fromJust $ findIndex (>x) xs
        
getFirstDownPos :: Int -> [Int] -> Int
getFirstDownPos _ [] = -1
getFirstDownPos _ [_] = -1
getFirstDownPos n (x:y:zs) = if x > y
                               then n
                               else getFirstDownPos (n+1) (y:zs)
