{-# LANGUAGE DeriveFunctor #-}

module OperatorParser
    ( OpTree(..)
    , Associativity(..)
    , op
    , foldTree
    , parseOperators
    , module Text.ParserCombinators.ReadP
    ) 
where

import Text.ParserCombinators.ReadP
import Control.Applicative hiding (many)

-- | Type for operator parse results. 'a' is the type of the operator, 'b'
-- | of the terms.
data OpTree a b = Op (OpTree a b) a (OpTree a b) 
                | Term b 
                deriving (Show, Eq, Functor)

-- | Type for specifying the assocativity of operators: left, right, or none.
data Associativity a = L a | R a | NoAssociativity a
                     deriving (Show, Eq, Functor)

-- | Transform an OpTree using the given function.
foldTree :: (a -> b -> b -> b) -> OpTree a b -> b
foldTree _ (Term x) = x
foldTree f (Op left o right) = f o (foldTree f left) (foldTree f right)



-- | Return a parser such that: given 'op s a', if s matches, the parser 
-- | returns a.
op :: String -> a -> ReadP a
op s x = x <$ string s

-- | Accept two arguments: 
-- | (1) A list of type [Associativity [ReadP a]], which contains parsers for
-- | operators (ReadP a). Each item of type Associativity [ReadP a] contains
-- | a group of operator parsers of the same precedence and associativity; 
-- | these groups are listed in order of precedence (lowest to highest).
-- | (2) A parser for the terms.
-- | And return a parser for operator expressions that yields a parse tree.

promoteOp :: ReadP a -> ReadP (OpTree a b -> OpTree a b -> OpTree a b)
promoteOp p = do
  skipSpaces
  o <- p
  skipSpaces
  return $ \a b -> Op a o b


chainMiddle :: ReadP a -> ReadP (a -> a -> a) -> ReadP a
chainMiddle p op = p >>= rest
  where rest x = do 
                   f <- op
                   y <- p
                   return $ f x y 
                 +++ return x

chainOp :: Associativity [ReadP a] -> ReadP (OpTree a b) -> ReadP (OpTree a b)
chainOp (L o) term = chainl1 term . foldr1 (+++) $ promoteOp <$> o
chainOp (R o) term = chainr1 term . foldr1 (+++) $ promoteOp <$> o
chainOp (NoAssociativity o) term = chainMiddle term . foldr1 (+++) $ promoteOp <$> o
                 

parens :: ReadP a -> ReadP a
parens p = do
  string "("
  skipSpaces
  r <- p
  skipSpaces
  string ")"
  return r

makeTermP :: ReadP b -> ReadP (OpTree a b)
makeTermP p = do
  t <- p
  skipSpaces
  return $ Term t

parseOperators :: [Associativity [ReadP a]] -> ReadP b -> ReadP (OpTree a b)
parseOperators arrOps termP = foldr chainOp term arrOps
  where term = (makeTermP termP) +++ parens (parseOperators arrOps termP)
