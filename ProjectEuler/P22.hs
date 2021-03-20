import System.IO
import Data.List
import Data.Char
import Data.List.Split -- cabal install split

nameScore :: String -> Int
nameScore = sum . map ((subtract 64) . ord)

doCalc :: String -> Int
doCalc c = sum . map (\(a,b) -> a*b) $ zip (map nameScore . sort . map (init . tail) . splitOn "," $ c) [1..]

main :: IO()
main = do
  handle <- openFile "p022_names.txt" ReadMode
  contents <- hGetContents handle
  let x = doCalc contents
  print x -- 871198282
  hClose handle
