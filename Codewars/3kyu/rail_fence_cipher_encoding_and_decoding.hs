-- https://www.codewars.com/kata/58c5577d61aefcf3ff000081
module RailFenceCipher.Kata (encode,decode) where

import Data.List (transpose, sortBy)
import Data.List.Split (chunksOf)
import Data.Function (on)

interleave :: [a] -> [a] -> [a]
interleave [] xs = xs
interleave xs [] = xs
interleave (x:xs) (y:ys) = x:y:(interleave xs ys)

encode :: [a] -> Int -> [a]
encode [] _ = []
encode xs n 
  | length xs <= n = xs
  | otherwise = (head codes) ++ (concat $ zipWith interleave code_body_left code_body_right) ++ (codes !! (n-1))
  where
    codes = transpose $ chunksOf (2*n-2) xs
    code_body_left = take (n-2) . drop 1 $ codes
    code_body_right = reverse . drop n $ codes

decode :: [a] -> Int -> [a]
decode [] _ = []
decode xs n = map snd . sortBy (compare `on` fst) $ zip (encode [1..length xs] n) xs
