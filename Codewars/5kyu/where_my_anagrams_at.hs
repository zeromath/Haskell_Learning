-- https://www.codewars.com/kata/523a86aa4230ebb5420001e1
module Anagram where

import Data.List

anagrams :: String -> [String] -> [String]
anagrams w ws = filter ((==s_w) . sort) ws
  where s_w = sort w
