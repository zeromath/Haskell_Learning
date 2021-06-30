-- https://www.codewars.com/kata/54120de842dff35232000195
module NIM where

import Data.Bits (xor)

-- | Returns the index and the number picked from a board
chooseMove :: [Int] -> (Int,Int)
chooseMove xs = head . filter ((>0) . snd) . zip [0..] $ zipWith (flip subtract) <*> map (xor nim) $ xs
  where
    nim = foldl1 xor xs
