-- https://www.codewars.com/kata/54e320dcebe1e583250008fd
module Codewars.Kata.Dec2Fact where
 
import Data.Char (ord, chr)
 
dec2FactString :: Integer -> String
dec2FactString 1 = "10"
dec2FactString n = reverse . map myShow $ calc [] n poss_factors
  where 
    poss_factors = reverse . takeWhile (<=n) $ 1:scanl1 (*) [1..]
    calc ans _ [] = ans
    calc ans rem (x:xs) = calc ((rem `div` x):ans) (rem `mod` x) xs
    
myShow :: Integer -> Char
myShow n
  | n `elem` [0..9] = chr . fromIntegral $ n + 48
  | otherwise = chr . fromIntegral $ n + 55


factString2Dec :: String -> Integer 
factString2Dec str = sum . zipWith (*) (reverse $ map myRead str) $ 1:scanl1 (*) [1..]

myRead :: Char -> Integer
myRead c
  | c `elem` ['0'..'9'] = toInteger $ (ord c) - 48
  | otherwise = toInteger $ (ord c) - 55
