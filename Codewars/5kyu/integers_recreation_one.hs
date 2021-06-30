-- https://www.codewars.com/kata/55aa075506463dac6600010d
module Codewars.G964.Sumdivsq where

listSquared :: Int -> Int -> [(Int, Int)]
listSquared m n = filter (isSquared . snd) $ zip <*> (sum . map (^2) . getDivisors <$>) $ [m..n]

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

isSquared :: Int -> Bool
isSquared = (==) <*> ((^2) . isqrt)

getDivisors :: Int -> [Int]
getDivisors x = (div x <$> small_divs) ++ if sq_x^2 == x
                                            then init small_divs
                                            else small_divs
  where
    sq_x = isqrt x
    small_divs = filter ((==0) . (mod x)) [1..sq_x]
