import Control.Monad (void)
import Control.Applicative (liftA2)
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void (Void)
import Control.Monad.Combinators.Expr
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

data RegExp = Normal Char       -- ^ A character that is not in "()*|."
            | Any               -- ^ Any character
            | ZeroOrMore RegExp -- ^ Zero or more occurances of the same regexp
            | Or RegExp RegExp  -- ^ A choice between 2 regexps
            | Str [RegExp]      -- ^ A sequence of regexps.
            deriving (Show, Eq)


-- helper funtion --
symbol = L.symbol space

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pNormalChar :: Parser RegExp
pNormalChar = Normal <$> noneOf "()*|."

pAny :: Parser RegExp
pAny = Any <$ char '.'

pOr :: Parser RegExp
pOr = Or <$> (pRest <* (char '|')) <*> pRest
  where pRest = try pZeroOrMore <|> try pStr <|> parens pExpr <|> pAny <|> pNormalChar

pZeroOrMore :: Parser RegExp
pZeroOrMore = ZeroOrMore <$> (pAny <|> pNormalChar <|> parens pExpr) <* (char '*')


pStr :: Parser RegExp
pStr = fmap Str . liftA2 (:) pRest $ some pRest
    where pRest = try pZeroOrMore <|> pAny <|> pNormalChar <|> (parens pExpr)

pExpr :: Parser RegExp
pExpr =  choice [try pOr
                , try pStr
                , try pZeroOrMore
                , pAny
                , pNormalChar
                , parens pExpr]

parseRegExp :: String -> Maybe RegExp
parseRegExp s = case parse (pExpr <* eof) undefined s of
  Left err -> Nothing
  Right a -> Just a
