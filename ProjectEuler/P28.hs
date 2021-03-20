import System.IO

diagSum :: Int -> Int
diagSum n = 8 * (n * (n + 1) * (2 * n + 1)) `div` 3 + 2 * n * (n + 1) + 4 * n + 1

main :: IO()
main = do
  print $ diagSum 500
