-- https://www.codewars.com/kata/52742f58faf5485cae000b9a
module FormatDuration where
import Data.List (intercalate)


formatDuration :: (Integral i) => i -> String
formatDuration n
  | n == 0 = "now"
  | length tokens == 1 = head tokens
  | otherwise = intercalate ", " (init tokens) ++ " and " ++ (last tokens)
  where
    rem = (0, toInteger n) : zipWith (\x y -> divMod (snd y) x) [31536000, 86400, 3600, 60, 1] rem
    tokens =  filter (/="") . zipWith parse ["", "year","day","hour","minute","second"] . map fst $ rem


parse :: String -> Integer -> String
parse _ 0 = ""
parse s 1 = "1 " ++ s
parse s x = show x ++ " " ++ s ++ "s"
