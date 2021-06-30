-- https://www.codewars.com/kata/52774a314c2333f0a7000688
module Codewars.Parentheses where

import Control.Monad

validParentheses :: String -> Bool
validParentheses s = (foldM (flip stack) [] s) == Just []

stack :: Char -> String -> Maybe String
stack c [] = if c == '(' 
             then Just ['('] 
             else Nothing
stack c s@(x:xs) = if c == ')' && x == '(' 
                   then Just xs 
                   else Just (c:s)
