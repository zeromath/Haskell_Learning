mymax :: (Ord a) => [a] -> a
mymax [] = error "WTF"
mymax [x] = x
mymax (x:xs)
  | x > maxtail = x
  | otherwise = maxtail
  where maxtail = mymax xs
