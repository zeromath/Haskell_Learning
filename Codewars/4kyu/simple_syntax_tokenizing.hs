-- https://www.codewars.com/kata/599a0d02755eae7070000079
module SimpleTokenizer (Token(..), tokenize) where

import qualified Text.Megaparsec as P
import Text.Megaparsec.Char
import Data.Void
import Control.Applicative hiding (some, many)

type Parser = P.Parsec Void String

data Token = Token String | Brackets [Token]
  deriving (Eq, Show)

tokenize :: String -> Maybe [Token]
tokenize "" = Just []
tokenize s = rightToMaybe $ P.parse (expr <* P.eof) undefined s
  where rightToMaybe = either (const Nothing) Just

tokenP :: Parser Token
tokenP = do
  xs <- (P.some letterChar) <|> (P.some (P.satisfy (`elem` operatorChars)))
  space
  return $ Token xs

tokenListP :: Parser Token
tokenListP = do
  char '('
  space
  xs <- P.many (tokenP <|> tokenListP)
  space
  char ')'
  space
  return $ Brackets xs

expr :: Parser [Token]
expr = P.some (tokenP <|> tokenListP)

operatorChars :: String
operatorChars = "!#$%&*+-/<=>@^_.,;"
