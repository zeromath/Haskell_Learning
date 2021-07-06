-- https://www.codewars.com/kata/52f831fa9d332c6591000511
module MoleculeToAtoms where

import Control.Applicative hiding (some, many, empty)
import Control.Arrow
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import qualified Data.Map as M

type Parser = Parsec Void String

data Atoms = Atom { atomName :: String, mltp :: Int}
           | AtomGroup {atomGroup :: [Atoms], mltp :: Int } deriving (Show)

elemName :: Parser String
elemName = (:) <$> upperChar <*> (((:[]) <$> lowerChar) <|> pure "")

elemMltp :: Parser Int
elemMltp = read <$> ((some numberChar) <|> return "1")

singleAtom :: Parser Atoms
singleAtom = Atom <$> elemName <*> elemMltp

groupAtom :: Char -> Char -> Parser Atoms
groupAtom op ed = AtomGroup <$> (char op *> expr <* char ed) <*> elemMltp

expr :: Parser [Atoms]
expr = some (singleAtom
             <|> groupAtom '(' ')'
             <|> groupAtom '[' ']'
             <|> groupAtom '{' '}')

unwind :: [Atoms] -> [(String, Int)]
unwind [] = []
unwind ((Atom n m):xs) = (n, m) : unwind xs
unwind ((AtomGroup ys m):xs) = (second (*m) <$> (unwind ys)) ++ unwind xs


parseMolecule :: String -> Either String [(String,Int)]
parseMolecule formula = case parse expr undefined formula of
  Left err -> Left "Not a valid molecule"
  Right d -> Right . M.toList $ foldl append M.empty (unwind d)
    where append = flip $ uncurry (M.insertWith (+))
