-- evalute "2+3" -> 5.0
evaluate :: String -> Double
evaluate s = expression [] ["="] ((parse s) ++ ["="])

parse :: String -> [String]
-- getParse nums -> whether "-" sign should be treated as negative sign -> the remaining expression
parse exp = getParse [] True exp
  where
    getParse [] _ [] = []
    getParse num _ [] = [num]
    getParse [] append_sign exp@(exp_head:exp_tail)
      | exp_head `elem` ['+', '*', '/', ')'] = [exp_head] : getParse [] False exp_tail
      | exp_head == '(' = [exp_head] : getParse [] True exp_tail
      | exp_head == '-' = if append_sign
                          then getParse "-" False exp_tail
                          else "-" : getParse [] False exp_tail
      | otherwise = getParse [exp_head] False exp_tail
    getParse num append_sign exp@(exp_head:exp_tail)
      | exp_head `elem` ['+', '*', '/', ')'] = num: [exp_head] : getParse [] False exp_tail
      | exp_head == '(' = num: [exp_head] : getParse [] True exp_tail
      | exp_head == '-' = if append_sign
                          then num : getParse "-" False exp_tail
                          else num : "-" : getParse [] False exp_tail
      | otherwise = getParse (num ++ [exp_head]) False exp_tail

{-
expression
    [list of nums to calc] ->
    [list of stacked operations] ->
    [list of unprocessed nums/ops] -> Result
-}
expression :: [Double] -> [String] -> [String] -> Double
expression nums _ []                              = head nums
expression nums ops@(ops_head:ops_tail) lists@(list_head:list_tail)
  -- every time we compare the new operator from the remaining list with the top operator in stack
  -- we always have a = in stack to force other operators to be calculated
  | list_head `elem` ["+", "-", "*", "/", "(", ")", "="] = case compareSymb ops_head list_head of
                                                      GT -> expression ((calc ops_head x y):ns) ops_tail lists
                                                      LT -> expression nums (list_head:ops) list_tail
                                                      EQ -> expression nums ops_tail list_tail
  | otherwise = expression ((read list_head :: Double):nums) ops list_tail
  where (x:y:ns) = nums

-- the actual calculation function
calc :: String -> Double -> Double -> Double
calc op x y = case op of
                "+" -> x + y
                "-" -> y - x
                "*" -> x * y
                "/" -> y / x

-- determine which operator (+, -, *, /, (, ), ) should calculate first
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
