-- https://www.codewars.com/kata/52449b062fb80683ec000024
module Codewars.Kata.Hashtag where

import Data.Char

generateHashtag :: String -> Maybe String
generateHashtag s
  | null list_of_words = Nothing
  | otherwise          = if length hashtag <= 140 then Just hashtag else Nothing
  where list_of_words = words s
        hashtag       = '#': (concat $ map upperFirst list_of_words)
        
upperFirst :: String -> String
upperFirst ""     = ""
upperFirst (c:cs) = toUpper(c):cs
