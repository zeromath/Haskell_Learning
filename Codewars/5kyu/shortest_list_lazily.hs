-- https://www.codewars.com/kata/585a8bd717cc9027e3000274
module ShortestList where

shortestList :: [[a]] -> [a]
shortestList [] = []
shortestList xs = snd . head . dropWhile ((==False) . fst) $ [(f x, x) | f <- (lengthAtMost <$> [0..]), x <-xs]
  where lengthAtMost n = null . drop n
