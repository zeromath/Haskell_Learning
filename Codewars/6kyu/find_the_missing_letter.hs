-- https://www.codewars.com/kata/5839edaa6754d6fec10000a2
module Kata where

findMissingLetter :: [Char] -> Char
findMissingLetter (x: y: cs)
  | y == succ x = findMissingLetter (y: cs)
  | otherwise = succ x
