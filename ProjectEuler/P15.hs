main :: IO()
main = do
  print ((product [21..40]) `div` (product [1..20])) -- 137846528820
