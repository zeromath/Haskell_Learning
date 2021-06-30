-- https://www.codewars.com/kata/5511b2f550906349a70004e1
module LastDigit where

lastDigit :: Integer -> Integer -> Integer
lastDigit a b
  | b == 0                = 1
  | elem r_a [0, 1, 5, 6] = r_a
  | elem r_a [4, 9]       = if odd b then r_a else mod (r_a ^ 2) 10
  | r_a == 8              = [6, 8, 4, 2] !! r_b4
  | r_a == 2              = [6, 2, 4, 8] !! r_b4 
  | r_a == 3              = [1, 3, 9, 7] !! r_b4
  | r_a == 7              = [1, 7, 9, 3] !! r_b4
  where r_a  = mod a 10
        r_b4 = fromInteger $ mod b 4
