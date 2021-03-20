import Data.Char

main :: IO()
main = do
  print . sum . map digitToInt . show $ (2^1000) -- 1366
