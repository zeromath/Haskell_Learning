import Data.Char

main :: IO()
main = do
  let x = sum . map digitToInt . show . product . take 100 $ [1..]
  print x -- 648
