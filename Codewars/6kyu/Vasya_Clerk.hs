-- https://www.codewars.com/kata/555615a77ebc7c2c8a0000b8
module VasyaClerk (tickets,CanHe(..)) where

type Money = Int
data CanHe = NO | YES deriving (Show,Eq)

tickets :: [Money] -> CanHe
tickets list_of_money = if search (0, 0) list_of_money then YES else NO
  where search (a, b) ms
          | null ms = True
          | otherwise = case head ms of
                          25  -> search (a + 1, b) (tail ms)
                          50  -> if a == 0 
                                 then False
                                 else search (a - 1, b + 1) (tail ms)
                          100 -> if b == 0
                                 then if a < 3
                                      then False
                                      else search (a - 3, b) (tail ms)
                                 else if a == 0
                                      then False
                                      else search (a - 1, b - 1) (tail ms)
