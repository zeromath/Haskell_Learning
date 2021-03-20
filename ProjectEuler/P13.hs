import System.IO

main :: IO()
main = do
  inputfile <- openFile "P13_data.txt" ReadMode
  content <- hGetContents inputfile
  print . take 10 . show . sum . map ((read::String->Int) . take 12) . lines $ content -- 5537376230
  hClose inputfile
