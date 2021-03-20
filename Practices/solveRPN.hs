solveRPN :: (Num a, Read a) => String -> a
solveRPN = head . foldl foldingFunction []. words
    where foldingFunction (x:y:seq) "*" = (x*y):seq
          foldingFunction (x:y:seq) "+" = (x+y):seq
          foldingFunction (x:y:seq) "-" = (y-x):seq
          foldingFunction seq nextNumber = read nextNumber:seq
