-- https://www.codewars.com/kata/52756e5ad454534f220001ef
module LongestCommonSubsequence where

lcs :: String -> String -> String
lcs "" _ = ""
lcs _ "" = ""
lcs xs@(x:xs') ys@(y:ys')
  | x == y = x : lcs xs' ys'
  | length xlcs > length ylcs = xlcs
  | otherwise = ylcs
  where xlcs = lcs xs ys'
        ylcs = lcs xs' ys
