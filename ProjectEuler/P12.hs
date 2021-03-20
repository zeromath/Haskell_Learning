import Math.Combinatorics.Multiset --cabal install multiset-comb

primes = [2] ++ filter (null . tail . primeFactors) [3, 5..]

primeFactors :: Int -> [Int]
primeFactors n = factors n primes
  where
    factors n pl@(p:ps)
      | p * p > n      = [n]
      | n `mod` p == 0 = p : factors (n `div` p) pl
      | otherwise      = factors n ps

factors :: Int -> Int
factors = product . map ((+1) . snd) . toCounts . fromList . primeFactors

main :: IO()
main = do
  print (take 1 . dropWhile (\x -> factors x < 500) . map (\x -> x * (x + 1) `div` 2) $ [1..]) !! 0 -- 76576500
