import Data.Char

factorial :: Int -> Int
factorial k = product [1..k]

sliceList :: Int -> [a] -> [a]
sliceList n = take 1 . drop n

codeList :: Int -> Int -> [Int] -> [Int]
codeList n m l
      | n == 1          = reverse (m:l)
      | m < factorial n = codeList (n - 1) m (0:l)
      | otherwise       = codeList (n - 1) r (q:l)
      where
        r = m `mod` (factorial n)
        q = m `div` (factorial n)

permutationCode :: Int -> Int -> [Int]
permutationCode n m = codeList (n - 1) (m - 1) []

deCode :: Int -> [Int] -> String
deCode n l = findCode l (map intToDigit [0..(n - 1)])
  where
    findCode l@(lf:lr) s
      | lr == []  = (sliceList lf s) ++ (sliceList (1 - lf) s)
      | otherwise = (sliceList lf s) ++ (findCode lr ((take lf s) ++ (drop (lf + 1) s)))

main :: IO()
main = do
  let x = deCode 10 . permutationCode 10 $ 1000000
  print x -- 2783915460
