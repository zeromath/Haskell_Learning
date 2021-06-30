-- https://www.codewars.com/kata/52bef5e3588c56132c0003bc
module TreeByLevels where

import TreeByLevels.TreeNode

import Data.Maybe (catMaybes)

treeByLevels :: Maybe (TreeNode a) -> [a]
treeByLevels Nothing = []
treeByLevels head = treeLevelTraverse [head]

treeLevelTraverse :: [Maybe (TreeNode a)] -> [a]
treeLevelTraverse [] = []
treeLevelTraverse xs = (catMaybes $ map (value <$>) xs) ++ (treeLevelTraverse . concat . (concat <$>) $ map ((\x->[left x,right x]) <$>) xs)
