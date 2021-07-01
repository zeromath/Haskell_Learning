module InsaneColouredTriangles (triangle) where

import qualified Data.Vector.Unboxed as V (Vector, (!), length, map, head)
import Control.Applicative (liftA2)

-- Say R, G, B = 0, 1, 2
-- Then the inductive formula is c = 2 * (a + b) `mod` 3
-- Inductively, the result of a sequence x_0 .. x_n is 
-- 2^n * ( sum (n \choose k) * x_k )
-- Hence, we can use Lucas's theorem


converter :: Char -> Int
converter 'R' = 0
converter 'G' = 1
converter _ = 2

reverseConverter :: Int -> Char
reverseConverter 0 = 'R'
reverseConverter 1 = 'G'
reverseConverter _ = 'B'

triangle :: V.Vector Char -> Char
triangle xs = if n == 0
                then V.head xs
                else reverseConverter . flip mod 3 . (*2^(n`mod` 2)) . sum $ zipWith (*) poss_nums coeffs
  where
    n = V.length xs - 1
    nums = V.map converter xs
    tri_exp = trinaryExpansion n
    poss_lists = getPossibleLists tri_exp
    poss_nums = map (\x -> (nums V.! x)) . map numOfTriExpansion $ poss_lists
    coeffs = map (binaryCoefficientMod3 tri_exp) poss_lists

binaryCoefficientMod3 :: [Int] -> [Int] -> Int
binaryCoefficientMod3 xs = flip mod 3 . product . zipWith binCoef xs
  where binCoef 2 1 = 2
        binCoef _ _ = 1

numOfTriExpansion :: [Int] -> Int
numOfTriExpansion = sum . zipWith (*) ((3^) <$> [0..])

getPossibleLists :: [Int] -> [[Int]]
getPossibleLists = foldr1 (liftA2 (++)) . map expand
  where expand x = (:[]) <$> [0..x]

trinaryExpansion :: Int -> [Int]
trinaryExpansion x
  | x == 0 = []
  | otherwise = (x `mod` 3) : trinaryExpansion (x `div` 3)
