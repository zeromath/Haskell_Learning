-- https://www.codewars.com/kata/5426d7a2c2c7784365000783
module Balanced.Parens where

import Data.List (inits)

parens = [""] : [ foldl1 (++) $ 
                  zipWith (\x y -> (++) <$> x <*> y) 
                  (map (('(':) <$>) pre) 
                  (map ((')':) <$>) $ reverse pre) | pre <- tail (inits parens) ]

balancedParens :: Int -> [String]
balancedParens n = parens !! n
