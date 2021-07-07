module JSON.Parser (parse) where

import qualified Text.Megaparsec as P
import Data.Void
import Text.Megaparsec.Char
import Text.Megaparsec.Debug

data Value = String String
           | Number Double
           | Object [(Value,Value)] -- an association list -- only a `String` is valid as the index `Value`
           | Array [Value]          -- not limited to identical primitive datatypes
           | Boolean Bool           -- either `True` or `False`
           | Null 
           deriving Show



type Parser = P.Parsec Void String

parseString :: Parser Value
parseString = String <$> ((char '"' *> P.many (P.noneOf "\"") <* char '"') 
                      P.<|> (char '\'' *> P.many (P.noneOf "'") <* char '\''))

parseNumber :: Parser Value
parseNumber = do
  s <- ((char '-' >> return negate) P.<|> return id)
  z <- digitChar
  x <- (P.many digitChar)
  y <- ((char '.' >> (P.some digitChar)) P.<|> return "0")
  if z /= '0' || null x
    then return . Number . s . read $ (z:x) ++ ('.':y)
    else fail "number?"
  
parseNull :: Parser Value
parseNull = do
  string "null"
  return Null
  
parseBoolean :: Parser Value
parseBoolean =  (do {string "false" ; return $ Boolean False}) P.<|> (do {string "true"; return $ Boolean True}) 

parseObject :: Parser Value
parseObject = do
  char '{'
  space
  xs <- (space *> kv_pairs <* space) `P.sepBy` (char ',')
  space
  char '}'
  return $ Object xs
    where 
      kv_pairs = do
        name <- parseString
        space
        char ':'
        space
        value <- expr
        return (name, value)

parseArray :: Parser Value
parseArray = do
  char '['
  space
  xs <- ((space *> expr <* space) `P.sepBy` (char ','))
  space
  char ']'
  return $ Array xs

expr :: Parser Value
expr = space *> P.choice [ parseString
              , parseNumber
              , parseNull
              , parseBoolean
              , dbg "pArray" parseArray
              , parseObject] <* space

parse :: String -> Maybe Value
parse s = case P.parse (expr <* P.eof) undefined s of
  Left err -> Nothing
  Right v -> Just v
