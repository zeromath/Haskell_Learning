-- https://www.codewars.com/kata/5518a860a73e708c0a000027
module LastDigit (lastDigit) where

lastDigit :: [Integer] -> Integer
lastDigit []     = 1
lastDigit [x]    = x `mod` 10
lastDigit [x, 0] = 1
lastDigit [x, y] = x_digits !! x10 !! y4
  where y4       = fromIntegral (y `mod` 4)
        x10      = fromIntegral (x `mod` 10)
        x_digits = [[0, 0, 0, 0],
                    [1, 1, 1, 1],
                    [6, 2, 4, 8],
                    [1, 3, 9, 7],
                    [6, 4, 6, 4],
                    [5, 5, 5, 5],
                    [6, 6, 6, 6],
                    [1, 7, 9, 3],
                    [6, 8, 4, 2],
                    [1, 9, 1, 9]]
lastDigit (x: y: z: xs)
  | isZero xs = lastDigit [x, y]
  | z == 0    = lastDigit [x]
  | z == 1    = lastDigit [x, y]
  | odd z     = lastDigit [x, y ^ 3]
  | otherwise = lastDigit [x, y ^ 2]
  where isZero xs
          | xs == []     = False
          | head xs /= 0 = False
          | otherwise    = not . isZero . tail $ xs
