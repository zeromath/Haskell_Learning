-- https://www.codewars.com/kata/52c4dd683bfd3b434c000292
module Awesome.Numbers where

import Data.Char

data Answer = No | Almost | Yes deriving (Show, Read, Eq, Ord)

isInteresting :: Integer -> [Integer] -> Answer
isInteresting n xs
  | n < 98                                             = No
  | (interestingNumber n) || (n `elem` xs)             = Yes
  | (interestingNumber (n + 1)) || ((n + 1) `elem` xs) = Almost
  | (interestingNumber (n + 2)) || ((n + 2) `elem` xs) = Almost
  | otherwise                                          = No

interestingNumber n
  | length list_of_digits < 3                                       = False
  | (head list_of_digits /= 0) && isSame (0: (tail list_of_digits)) = True
  | isSame list_of_digits                                           = True
  | isInc list_of_digits                                            = True
  | isDec list_of_digits                                            = True
  | isPal list_of_digits                                            = True
  | otherwise = False
  where list_of_digits = map digitToInt . show $ n
        isSame (x:xs) = null . filter (/= x) $ xs
        isInc xs
          | last xs == 0 && (isSame $ zipWith (-) (reverse xs) (0:[9,8..1])) = True
          | isSame $ (zipWith (-) xs [1..9])                                 = True
          | otherwise                                                        = False
        isDec xs
          | last xs == 0 && (isSame $ zipWith (-) (reverse xs) [0..9]) = True
          | isSame $ (zipWith (-) xs [9,8..1])                         = True
          | otherwise                                                  = False
        isPal xs
          | length xs < 2      = True
          | head xs /= last xs = False
          | otherwise          = isPal . init . tail $ xs
        
