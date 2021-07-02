module ApplicativeParser where

import Data.Char
import Prelude hiding (fmap)

-- | An ambiguous parser.
newtype Parser a = P { unP :: String -> [(String, a)] }

-- | Change the result of a parser.
pmap :: (a -> b) -> Parser a -> Parser b
pmap f p = P $ map (uncurry ((.f) . (,))) . unP p

-- | Operator version of 'pmap'.
(<#>) :: (a -> b) -> Parser a -> Parser b
(<#>) = pmap

-- | Parse a value and replace it.
(<#) :: a -> Parser b -> Parser a
(<#) a p = P $ \s -> map (\(x,y) -> (x,a)) $ (unP p) s

infixl 4 <#>
infixl 4 <#

-- | Parse a character only when a predicate matches.
predP :: (Char -> Bool) -> Parser Char
predP p = P $ \s -> case s of
  [] -> []
  (c:cs) -> if p c then [(cs, c)] else []


-- | Succeed only when parsing the given character.
charP :: Char -> Parser Char
charP c = P $ \s -> case s of
  [] -> []
  (c1:cs) -> if c1 == c then [(cs, c)] else []

-- | Inject a value into an identity parser.
inject :: a -> Parser a
inject x = P $ \s -> [(s, x)]

-- | Given a parser with a function value and another parser, parse the function
-- first and then the value, return a parser which applies the function to the
-- value.
(<@>) :: Parser (a -> b) -> Parser a -> Parser b
pf <@> px = P $ \s -> [(r2, f x) | (r, f) <- (unP pf) s, (r2, x) <- (unP px) r]

(<@) :: Parser a -> Parser b -> Parser a
pa <@ pb = P $ \s -> [(r2, f) | (r, f) <- (unP pa) s, (r2, g) <- (unP pb) r]

(@>) :: Parser a -> Parser b -> Parser b
pa @> pb = P $ \s -> [(r2, g) | (r, f) <- (unP pa) s, (r2, g) <- (unP pb) r]

infixl 4 <@
infixl 4 @>
infixl 4 <@>

-- | Parse a whole string.
stringP :: String -> Parser String
stringP [] = P $ \s -> [(s, "")]
stringP (c:cs) = (:) <#> (charP c) <@> (stringP cs)

-- | Construct a parser that never parses anything.
emptyP :: Parser a
emptyP = P $ \s -> []

-- | Combine two parsers: When given an input, provide the results of both parser run on the input.
(<<>>) :: Parser a -> Parser a -> Parser a
(<<>>) f g = P $ \s -> (unP f) s ++ (unP g) s

infixl 3 <<>>

-- | Apply the parser zero or more times.
many :: Parser a -> Parser [a]
many p = P $ \s -> case (unP p) s of
  [] -> [(s, [])]
  xs -> [(rems2, res:res2) | (rems, res) <- xs, (rems2, res2) <- (unP (many p)) rems]

-- | Apply the parser one or more times.
some :: Parser a -> Parser [a]
some p = P $ \s -> case (unP p) s of
  [] -> []
  xs -> [(rems2, res:res2) | (rems, res) <- xs, (rems2, res2) <- (unP (many p)) rems]


-- | Apply a parser and return all ambiguous results, but only those where the input was fully consumed.
runParser :: Parser a -> String -> [a]
runParser p cs = [res | (rems, res) <- (unP p) cs, rems /= cs]

-- | Apply a parser and only return a result, if there was only one unambiguous result with output fully consumed.
runParserUnique :: Parser a -> String -> Maybe a
runParserUnique p cs = case [res | (rems, res) <- (unP p) cs, rems == []] of
  [x] -> Just x
  _ -> Nothing

-- | Kinds of binary operators.
data BinOp = AddBO | MulBO deriving (Eq, Show)

-- | Some kind of arithmetic expression.
data Expr = ConstE Int
          | BinOpE BinOp Expr Expr
          | NegE Expr
          | ZeroE
          deriving (Eq, Show)

evalExpr :: Expr -> Int
evalExpr (ConstE e) = e
evalExpr (NegE f) = -(evalExpr f)
evalExpr ZeroE = 0
evalExpr (BinOpE AddBO a b) = (evalExpr a) + (evalExpr b)
evalExpr (BinOpE MulBO a b) = (evalExpr a) * (evalExpr b)

-- | Parse arithmetic expressions, with the following grammar:
--
--     expr         ::= const | binOpExpr | neg | zero
--     const        ::= int
--     binOpExpr    ::= '(' expr ' ' binOp ' ' expr ')'
--     binOp        ::= '+' | '*'
--     neg          ::= '-' expr
--     zero         ::= 'z'
-- 

digit :: Parser Char
digit = predP (`elem` "0123456789")

cst :: Parser Expr
cst = ConstE . read <#> (some digit)

binOp :: Parser BinOp
binOp = (AddBO <# charP '+') <<>> (MulBO <# charP '*')

binOpExpr :: Parser Expr
binOpExpr = ((flip ($)) <#> ((charP '(') @> expr <@ (charP ' '))) <@> (BinOpE <#> binOp) <@> ((charP ' ') @> expr <@ (charP ')'))

zero :: Parser Expr
zero =  ZeroE <# charP 'z'

neg :: Parser Expr
neg = NegE <#> (charP '-' @> expr)

expr :: Parser Expr
expr = neg <<>> binOpExpr <<>> zero <<>> cst


parseExpr :: String -> Maybe Expr
parseExpr cs = runParserUnique expr cs