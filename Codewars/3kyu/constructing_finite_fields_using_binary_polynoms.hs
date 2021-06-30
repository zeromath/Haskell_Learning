-- https://www.codewars.com/kata/54f1b7b3f58ba8ee720005a8
module PolynomField where

import Data.List (intercalate)
import Data.Bits (xor, shiftL)

data BinaryPolynom = BinaryPolynom { terms :: Int} deriving (Eq)

toPoly :: Int -> [Int]
toPoly 0 = []
toPoly n = (n `mod` 2): toPoly (n `div` 2)

polyToTerms :: [Int] -> [Int]
polyToTerms = map (flip(-) 1) . filter (/=0) . zipWith (*) [1..]

zero, one :: BinaryPolynom
zero = BinaryPolynom 0
one  = BinaryPolynom 1

deg :: BinaryPolynom -> Int
deg = (flip(-) 1) . length . toPoly . terms

-- | Constructs a monom with the given degree.
polyFromDeg :: Int -> BinaryPolynom
polyFromDeg n
  | n == -1   = zero
  | otherwise = BinaryPolynom (2^n)

polyFromPowers :: [Int] -> BinaryPolynom
polyFromPowers = BinaryPolynom . sum . map (2^)

instance Show BinaryPolynom where
  show (BinaryPolynom 0) = "0"
  show p                 = intercalate " + " . reverse $ if even n then s else "1":s
    where n = terms p
          s = map (("x^" ++ ). show) . filter (/=0) . tail . zipWith (*) [0..] . toPoly $ n

-- | Multiplication in the polynom ring.
multiply :: BinaryPolynom -> BinaryPolynom -> BinaryPolynom
multiply (BinaryPolynom x) = BinaryPolynom . foldl xor 0 . map (shiftL x) . polyToTerms . toPoly . terms

-- | Addition and multiplication in the polynom field.
(.+.), (.*.) :: BinaryPolynom -> BinaryPolynom -> BinaryPolynom
(BinaryPolynom x) .+. (BinaryPolynom y) = BinaryPolynom (xor x y)
(.*.) x = snd . flip polyDivMod (polyFromPowers [0,1,3,4,8]) . multiply x

polyDivMod :: BinaryPolynom -> BinaryPolynom -> (BinaryPolynom, BinaryPolynom)
polyDivMod x y
  | deg x < deg y = (zero, x)
  | otherwise     = (quot .+. next_q, next_r)
  where quot             = polyFromDeg ((deg x) - (deg y))
        (next_q, next_r) = polyDivMod (x .+. (multiply y quot)) y
