-- https://www.codewars.com/kata/549ee8b47111a81214000941
module ShortestKnightPath.Kata (knight) where
import Data.Char (ord, chr)

knight :: String -> String -> Int
knight s e = bfs e [] [(s, 0)]

bfs :: String -> [String] -> [(String, Int)] -> Int
bfs end_state visited (x:xs)
  | (fst x) == end_state = snd x
  | (fst x) `elem` visited = bfs end_state visited xs
  | otherwise = bfs end_state visited (xs ++ new_positions)
  where new_positions = zip (getNewPos $ fst x) (repeat $ snd x + 1)

getNewPos :: String -> [String]
getNewPos [x, y] = ((:) <$> (getNextChar 2 x) <*> (getNextNum 1 y)) 
                    ++ ((:) <$> (getNextChar 1 x) <*> (getNextNum 2 y))


getNextNum :: Int -> Char -> [String]
getNextNum n '1' = if n == 1 then ["2"] else ["3"];
getNextNum n '8' = if n == 1 then ["7"] else ["6"];
getNextNum 2 '2' = ["4"]
getNextNum 2 '7' = ["5"]
getNextNum n c = [[chr (ord c - n)], [chr (ord c + n)]]

getNextChar :: Int -> Char -> String
getNextChar n 'a' = if n == 1 then "b" else "c";
getNextChar n 'h' = if n == 1 then "g" else "f";
getNextChar 2 'b' = "d"
getNextChar 2 'g' = "e"
getNextChar n c = [chr (ord c - n), chr (ord c + n)]
