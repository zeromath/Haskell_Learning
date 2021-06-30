-- https://www.codewars.com/kata/5a25ac6ac5e284cfbe000111
module ColouredTriangle.Kata where
triangle :: String -> String
triangle x = iterate nextLevel x !! (length x - 1)



nextLevel :: String -> String
nextLevel s@(x:xs) = zipWith generate s xs
  where generate a b = if a == b
                       then a
                       else other a b
                       
other :: Char -> Char -> Char
other 'R' 'G' = 'B'
other 'G' 'R' = 'B'
other 'B' 'R' = 'G'
other 'R' 'B' = 'G'
other 'B' 'G' = 'R'
other 'G' 'B' = 'R'
