-- https://www.codewars.com/kata/56606694ec01347ce800001b
module Codewars.Triangles where

import Data.List (sort)

isTriangle :: Int -> Int -> Int -> Bool
isTriangle a b c = and [a + b > c, a + c > b, b + c > a]
