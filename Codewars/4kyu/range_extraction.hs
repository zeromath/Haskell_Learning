-- https://www.codewars.com/kata/51ba717bb08c1cd60f00002f
module RangeExtractor.JorgeVS.Kata where
solution :: [Int] -> String
solution [] = ""
solution [x] = show x
solution [x,y] = (show x) ++ "," ++ (show y)
solution (x:y:xs)
  | x + 1 /= y       = show x ++ "," ++ solution (y:xs)
  | y + 1 /= head xs = show x ++ "," ++ show y ++ "," ++ solution xs
  | otherwise        = findBounds x (head xs) (tail xs)
  where findBounds start current xs
          | xs == []               = show start ++ "-" ++ show current
          | current + 1 /= head xs = show start ++ "-" ++ show current ++ "," ++ solution xs
          | otherwise              = findBounds start (head xs) (tail xs)
