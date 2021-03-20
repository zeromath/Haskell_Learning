data ComparablePair = ComparablePair{ lambda::Int, number::Int} deriving(Show,Eq)

instance Ord ComparablePair where
  ComparablePair a1 _ <= ComparablePair a2 _ = a1 <= a2

findQuotient :: Int -> ComparablePair
findQuotient d = ComparablePair (length $ doCalc d 10 []) d
  where
    doCalc d r l
      | r == 0 = l
      | r1 `elem` l = l           
      | d > r = doCalc d (10*r) (r:l)
      | otherwise = doCalc d (10*r1) (r1:l)
      where
        r1 = r `mod` d

main :: IO()
main = do
  print . number . maximum . map findQuotient $ [2..999] -- 983
