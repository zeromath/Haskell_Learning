-- https://www.codewars.com/kata/55911ef14065454c75000062
module MultNumAsStrings where

import Data.Char (intToDigit, digitToInt)
import Data.List (inits)

-- | mulitply two numbers as strings
multiply :: String -> String -> String
multiply xs ys = foldl1 (++) . map show . truncateZero $ mul num_xs num_ys
    where
        num_xs = digitToInt <$> xs
        num_ys = digitToInt <$> ys
        
truncateZero :: [Int] -> [Int]
truncateZero [] = [0]
truncateZero (0:xs) = truncateZero xs
truncateZero xs = xs
        
mul :: [Int] -> [Int] -> [Int]
mul xs = reverse . reduceDigits 0 . foldl1 add . zipWith (++) (inits [0, 0..]) . (oneMulti (reverse xs) <$>) . reverse

add :: [Int] -> [Int] -> [Int]
add [] ys = ys
add xs [] = xs
add (x:xs) (y:ys) = x + y : (add xs ys)


oneMulti :: [Int] -> Int -> [Int]
oneMulti ys x = map (x*) $ ys

reduceDigits :: Int -> [Int] -> [Int]
reduceDigits carry [] = if carry /= 0  then [carry] else []
reduceDigits carry (y:ys) = let total = y + carry in total `mod` 10 : (reduceDigits (total `div` 10) ys)
