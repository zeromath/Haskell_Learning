import Data.Char (toLower)

solvePassword :: String -> [[Char]]
solvePassword = map stringToLower $ take 25 $ iterate solve "VRPHWLPHV L ZDQW WR FKDW ZLWK BRX,EXW L KDYH QR UHDVRQ WR FKDW ZLWK BRX"
  where solve = map (\x -> if x `elem` ['A'..'Z'] then mysucc x else x )
        mysucc x = if x=='Z' then 'A' else succ x
        stringToLower = map toLower
