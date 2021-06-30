-- https://www.codewars.com/kata/52a78825cdfc2cfc87000005
module EvaluateMathematicalExpression (calc) where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)
import Data.Functor.Identity

lexer = makeTokenParser emptyDef

-- data --

data Expr = Real Double | Num Int | Add Expr Expr
          | Sub Expr Expr | Mul Expr Expr | Div Expr Expr | Neg Expr
  deriving Show

-- parse --

expr :: Parsec String () Expr
expr = buildExpressionParser table term

table = [[nestedUnary "-" Neg],
         [binary "*" Mul AssocLeft,
          binary "/" Div AssocLeft],
         [binary "+" Add AssocLeft,
          binary "-" Sub AssocLeft]]
  where
    binary name fun assoc = Infix (fun <$ reservedOp lexer name) assoc
    unary  name fun = Prefix $ fun <$ reservedOp lexer name
    nestedUnary name fun = Prefix $ foldr1 (.) <$> many1 (fun <$ reservedOp lexer name)

term :: Parsec String () Expr
term = parens lexer expr
     <|> try (lexeme lexer reals)
     <|> lexeme lexer number

reals :: Parsec String () Expr
reals = do { whiteSpace lexer
           ; s <- (char '-' >> return negate) <|> (optional (char '+') >> return id)
           ; x <- many digit
           ; char '.'
           ; y <- many1 digit
           ; return . Real . s . read $ (if null x then "0" else x) ++ ('.':y)
           }

number :: Parsec String () Expr
number = do{ whiteSpace lexer 
           ; x <- many1 digit
           ; return . Num . read $ x
           } <?> "number"

-- eval --

eval :: Expr -> Double
eval (Real x) = x
eval (Num x) = fromIntegral x :: Double
eval (Add x y) = (eval x) + (eval y)
eval (Sub x y) = (eval x) - (eval y)
eval (Mul x y) = (eval x) * (eval y)
eval (Div x y) = (eval x) / (eval y)
eval (Neg x) = -(eval x)


-- preprocess --

preprocess :: String -> String
preprocess "" = ""
preprocess (c:s) = if c == '-'
                      then " -" ++ preprocess s
                      else c: preprocess s

deleteWhiteChar :: String -> String
deleteWhiteChar cs@(c:s)
  | c `elem` [' ', '\t', '\n'] = deleteWhiteChar s
  | otherwise = cs

calc :: String -> Double
calc s = case parse expr "" (deleteWhiteChar . preprocess $ s) of
  Left err -> 0.0
  Right t -> eval t
