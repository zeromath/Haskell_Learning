-- https://www.codewars.com/kata/5263a84ffcadb968b6000513
module MatrixMul where

import Control.Applicative

type Mat a = [[a]]

matMul :: Num a=> Mat a -> Mat a -> Mat a
matMul a1 b1 = (\x -> sum . zipWith (*) x <$> transpose b1) <$> a1
  where transpose = getZipList . traverse ZipList
