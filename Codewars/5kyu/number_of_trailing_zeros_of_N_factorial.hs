-- https://www.codewars.com/kata/52f787eb172a8b4ae1000a34
module Zeros where

zeros :: Int -> Int
zeros n = min (getPower n 2) (getPower n 5)

getPower :: Int -> Int -> Int
getPower n p = sum . map (div n) . takeWhile (<=n) $ (p^) <$> [1..] 
