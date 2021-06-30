-- https://www.codewars.com/kata/547202bdf7587835d9000c46
{-# LANGUAGE NoImplicitPrelude #-}
module Monads where

import Prelude hiding (Monad, Identity, Maybe(..), State, Reader, Writer)
import Data.Monoid

class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

data Identity a = Identity a
  deriving (Show, Eq)

data Maybe a = Nothing | Just a
  deriving (Show, Eq)

data State s a = State {runState :: s -> (a, s)}

data Reader s a = Reader {runReader :: s -> a }

data Writer w a = Writer {runWriter :: (w, a)}

instance Monad Identity where
  return a = Identity a
  (Identity v) >>= f = f v

instance Monad Maybe where
  return = Just
  Nothing >>= f = Nothing
  (Just v) >>= f = f v

instance Monad (State s) where
  return x = State $ \s -> (x, s)
  (State g) >>= f = State $ \st ->
                              let (x, st') = g st
                              in runState (f x) st'  
  
  

instance Monad (Reader s) where
  return x = Reader $ \e -> x
  (Reader g) >>= f = Reader $ \e -> runReader (f (g e)) e

instance Monoid w => Monad (Writer w) where
  return a = Writer (mempty, a)
  (Writer (v, s)) >>= f = let (Writer (v', y)) = f s in Writer (v `mappend` v', y)
