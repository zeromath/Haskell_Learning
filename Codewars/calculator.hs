module Calculator where

evaluate :: String -> Double
evaluate s = expression [] ["="] lists
  where lists = (words s) ++ ["="]

expression :: [Double] -> [String] -> [String] -> Double
expression nums _ []                              = head nums
expression nums ops@(oh:ot) lists@(lh:lt)
  | lh `elem` ["+", "-", "*", "/", "(", ")", "="] = case compareSymb oh lh of
                                                      GT -> expression ((calc oh x y):ns) ot lists
                                                      LT -> expression nums (lh:ops) lt
                                                      EQ -> expression nums ot lt
  | otherwise                                     = expression ((read lh :: Double):nums) ops lt
  where (x:y:ns) = nums

calc :: String -> Double -> Double -> Double
calc op x y = case op of
                "+" -> x + y
                "-" -> y - x
                "*" -> x * y
                "/" -> y / x

compareSymb :: String -> String -> Ordering
compareSymb s1 s2
  | s1 == "=" && s2 == "=" = EQ
  | s1 == "="              = LT
  | s2 == "="              = GT
  | s1 == "(" && s2 == ")" = EQ
  | s1 == "(" || s2 == "(" = LT
  | s1 == ")" || s2 == ")" = GT
  | s1 `elem` ["*", "/"]   = GT
  | s2 `elem` ["*", "/"]   = LT
  | otherwise              = GT