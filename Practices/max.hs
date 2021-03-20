mymax :: (Ord a) => [a] -> a
mymax [] = error "Empty List"
mymax [x] = x
mymax (x:xs)
  | x > maxtail = x
  | otherwise = maxtail
  where maxtail = mymax xs
