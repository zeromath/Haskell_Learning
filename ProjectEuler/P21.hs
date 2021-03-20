factors :: Int -> Int
factors 1 = 1
factors n = 1 + sum list_of_small_factors + (sum . map (n `div`) $ list_of_small_factors) - extra_factor
  where
    m = round(sqrt(fromIntegral(n)))
    list_of_small_factors = filter (\x -> n `mod` x == 0) [2..m]
    extra_factor
      | m^2 == n  = m
      | otherwise = 0

isAmicable :: Int -> Bool
isAmicable n
  | m == n = False
  | n == factors m = True
  | otherwise = False
  where m = factors n

main :: IO()
main = do
  let x = sum . filter isAmicable . take 10000 $ [1..]
  print x -- 31626
