module LispLovesMe where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Debug
import Data.Void
import Data.Maybe
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

mySpaces = " ,\n\t\r"

parens :: Parser a -> Parser a
--parens = between (char '(') (char ')')
parens p = do
  char '('
  space
  m <- p
  space
  char ')'
  return m

data AST = I32 Int
         | Sym String
         | Nul
         | Err
         | Lst [AST]
         | Boo Bool
         | Nod AST [AST]
         deriving (Eq)

instance Show AST where
  show (I32 n) = show n
  show (Sym s) = s
  show Nul = "null"
  show Err = "error"
  show (Boo b) = if b then "true" else "false"
  show (Nod f xs) = "(" ++ (unwords $ map show (f:xs)) ++ ")"
  show (Lst xs) = "[" ++ (unwords $ map show xs) ++ "]"

pI32 :: Parser AST
pI32 = I32 . read <$> some digitChar

pNull :: Parser AST
pNull = (Nul <$ string "null") <|> (Nul <$ (char '(' *> space <* char ')'))

pBoo :: Parser AST
pBoo = (Boo False <$ string "false") <|> (Boo True <$ string "true")

pSym :: Parser AST
pSym = do
  b <- (noneOf $ " ,\n\t\r()" ++ ['0'..'9'])
  res <- many (noneOf " ,\n\t\r()")
  return . Sym $ b:res

pNodes :: Parser AST
pNodes = parens $ do
  h <- pExpr
  Lst t <- pList
  return $ Nod h t

pExpr :: Parser AST
pExpr = space *> (try pNull
      <|> try pBoo
      <|> pSym
      <|> pI32
      <|> pNodes)

pList :: Parser AST
pList = Lst <$> many (try (try (space1 *> pExpr) <|> pNodes))


preludeFunctions :: [(String, [AST] -> AST)]
preludeFunctions =
  [ ("+", numLongOp (+))
  , ("*", numLongOp (*))
  , ("-", numLongOp (-))
  , ("/", numLongOp div)
  , ("^", numBiOp (^))
  , (">", numBoolBinop (>))
  , ("<", numBoolBinop (<))
  , ("!", boolSiOp (not))
  , ("list", Lst)
  , ("size", I32 . length)
  , ("reverse", lispReverse)
  , ("..", expand)
  , ("==", numBoolBinop (==))
  , (">=", numBoolBinop (>=))
  , ("<=", numBoolBinop (<=))
  , ("!=", numBoolBinop (/=))
  , ("if", lispIf)
  ]



lispReverse :: [AST] -> AST
lispReverse [(Lst ys)] = Lst $ reverse ys
lispReverse _ = Err

expand :: [AST] -> AST
expand xs
  | length xs /= 2 = Err
  | otherwise = Lst $ I32 <$> [x..y]
  where [x, y] = unpackNum <$> xs

lispIf :: [AST] -> AST
lispIf [a, b, c] = if (unpackBool a)
                then b
                else c
lispIf [a, b] = lispIf [a, b, Nul]
lispIf _ = Err



numLongOp :: (Int -> Int -> Int) -> [AST] -> AST
numLongOp op xs
  | length xs < 2 = Err
  | otherwise = I32 $ foldl1 op $ map unpackNum xs


numBiOp :: (Int -> Int -> Int) -> [AST] -> AST
numBiOp op xs
  | length xs /= 2 = Err
  | otherwise = I32 $ (ys !! 0) `op` (ys !! 1)
  where
    ys = unpackNum <$> xs


boolSiOp :: (Bool -> Bool) -> [AST] -> AST
boolSiOp op xs
  | length xs /= 1 = Err
  | otherwise = Boo $ op ys
  where ys = unpackBool (xs !! 0)

unpackBool :: AST -> Bool
unpackBool (Boo x) = x
unpackBool x = unpackBool . eval $ x

numBoolBinop :: (Int -> Int -> Bool) -> [AST] -> AST
numBoolBinop op xs
  | length xs /= 2 = Err
  | otherwise = Boo $ op (ys !! 0) (ys !! 1)
  where ys = unpackNum <$> xs


unpackNum :: AST -> Int
unpackNum (I32 n) = n
unpackNum x = unpackNum . eval $ x


--

lispPretty :: String -> Maybe String
lispPretty s = case parse pExpr "" s of
  Left _ -> Nothing
  Right val -> Just . show $ val

lispEval :: String -> Maybe AST
lispEval s = case parse pExpr "" s of
  Left _ -> Nothing
  Right val -> Just $ eval val

eval :: AST -> AST
eval (Nod (Sym f) xs) = (fromMaybe (\_ -> Err) (lookup f preludeFunctions)) $ eval <$> xs
eval x = x
