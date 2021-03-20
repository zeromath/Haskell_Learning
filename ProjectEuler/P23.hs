import Data.List
import Data.List.Ordered -- cabal install data-ordlist

factors :: Int -> Int
factors 1 = 1
factors n = 1 + sum list_of_small_factors + (sum . map (n `div`) $ list_of_small_factors) - extra_factor
  where
    m = round(sqrt(fromIntegral(n)))
    list_of_small_factors = filter (\x -> n `mod` x == 0) [2..m]
    extra_factor
      | m^2 == n  = m
      | otherwise = 0

abundant_number_list = filter (\x -> x < factors x) . take 28123 $ [1..]

sum_of_abun_list = takeWhile (<28123) . nubSort $ (+) <$> abundant_number_list <*> abundant_number_list

main :: IO()
main = do
 let x = 395437503 - sum(sum_of_abun_list)
 print x -- 4179871
