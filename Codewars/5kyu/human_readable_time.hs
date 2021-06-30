-- https://www.codewars.com/kata/52685f7382004e774f0001f7
module HumanTime where

import Data.List (intercalate)

humanReadable :: Int -> String
humanReadable n = intercalate ":" . map (myshow . fst) . tail $ scanl (\x -> divMod (snd x)) (1, n) [3600, 60, 1]
   where myshow x
           | x < 10 = '0': show x
           | otherwise = show x
