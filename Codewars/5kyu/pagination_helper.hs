-- https://www.codewars.com/kata/515bb423de843ea99400000a
module Codewars.Kata.Pagination where

type Collection a = [a]
type ItemsPerPage = Int

itemCount :: Collection a -> Int
itemCount = length

pageCount :: Collection a -> ItemsPerPage -> Int
pageCount xs n = (div l n) + if mod l n == 0 then 0 else 1
  where l = length xs

pageItemCount :: Collection a -> ItemsPerPage -> Int -> Maybe Int
pageItemCount xs n page
  | page < 0 = Nothing
  | page < (div l n) = Just n
  | page == (div l n) && (mod l n) /= 0 = Just $ mod l n
  | otherwise = Nothing
  where l = length xs

pageIndex :: Collection a -> ItemsPerPage -> Int -> Maybe Int
pageIndex xs n item
  | l == 0 = Nothing -- meaningless bugs
  | item >= l = Nothing
  | item < 0 = Just 0 -- meaningless bugs
  | otherwise = Just $ div item n
  where l = length xs
