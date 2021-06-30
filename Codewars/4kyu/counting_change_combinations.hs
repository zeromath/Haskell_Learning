-- https://www.codewars.com/kata/541af676b589989aed0009e7
module Change where

countChange :: Integer -> [Integer] -> Integer
countChange 0 _ = 1
countChange _ [] = 0
countChange t (x:xs) = sum $ flip countChange xs <$> rems
  where rems = map (t-) $ takeWhile (<=t) $ (x*) <$> [0..]
