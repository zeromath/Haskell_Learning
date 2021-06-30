-- https://www.codewars.com/kata/5c302f562f6fe300155a1933
{-# LANGUAGE TypeOperators, TypeFamilies, GADTs, UndecidableInstances #-}

module Kata.TimesComm where

import Kata.TimesComm.Definitions

{- Preloaded code. Maybe helpful for local editing.

-- | The natural numbers, encoded in types.
data Z
data S n

-- | Predicate describing natural numbers.
-- | This allows us to reason with `Nat`s.
data Natural :: * -> * where
  NumZ :: Natural Z
  NumS :: Natural n -> Natural (S n)

-- | Predicate describing equality of natural numbers.
data Equal :: * -> * -> * where
  EqlZ :: Equal Z Z
  EqlS :: Equal n m -> Equal (S n) (S m)

-- | Peano definition of addition.
type family (:+:) (n :: *) (m :: *) :: *
type instance Z :+: m = m
type instance S n :+: m = S (n :+: m)

-- | Peano definition of multiplication.
type family (:*:) (n :: *) (m :: *) :: *
type instance Z :*: m = Z
type instance S n :*: m = m :+: (n :*: m)

-}

reflexive :: Natural n -> Equal n n
reflexive NumZ     = EqlZ
reflexive (NumS n) = EqlS . reflexive $ n

-- | if a = b, then b = a.
symmetric :: Equal a b -> Equal b a
symmetric EqlZ     = EqlZ
symmetric (EqlS x) = EqlS . symmetric $ x

-- | if a = b and b = c, then a = c.
transitive :: Equal a b -> Equal b c -> Equal a c
transitive EqlZ EqlZ         = EqlZ
transitive (EqlS x) (EqlS y) = EqlS $ transitive x y


-- This will be helpful
equality :: Equal n m -> Equal a b -> Equal (n :+: a) (m :+: b)
equality EqlZ z = z
equality (EqlS a) z = EqlS $ equality a z

increaseByA ::  Natural a -> Equal n m -> Equal (a :+: n) (a :+: m)
increaseByA a z = equality (reflexive a) z

assocLemma :: Natural a -> Natural b -> Equal n m -> Equal (a :+: (b :+: n)) ((a :+: b) :+: m)
assocLemma NumZ b z = increaseByA b z
assocLemma (NumS a) b z   = EqlS $ assocLemma a b z

-- | You need this! Copy your solution from
-- https://www.codewars.com/kata/a-plus-b-plus-c-equals-a-plus-b-plus-c-prove-it/haskell
plusAssoc :: Natural a -> Natural b -> Natural c -> Equal (a :+: (b :+: c)) ((a :+: b) :+: c)
plusAssoc a b c = assocLemma a b (reflexive c)

-- | You need this! Copy your solution from
-- https://www.codewars.com/kata/a-plus-b-equals-b-plus-a-prove-it/haskell
plusCommutes :: Natural a -> Natural b -> Equal (a :+: b) (b :+: a)
plusCommutes NumZ NumZ     = EqlZ
plusCommutes NumZ (NumS b) = EqlS $ plusCommutes NumZ b
plusCommutes a NumZ        = symmetric $ plusCommutes NumZ a
plusCommutes a (NumS b)    = transitive (plusLemma a b) (EqlS $ plusCommutes a b)

-- Prove a lemma about :+:
-- | a :+: S b = S a :+: b
plusLemma :: Natural a -> Natural b -> Equal (a :+: (S b)) (S (a :+: b))
plusLemma NumZ b     = EqlS . reflexive $ b
plusLemma (NumS a) b = EqlS $ plusLemma a b

-- This will also be helpful
--zeroComm :: Natural a -> Equal Z (a :*: Z)
--zeroComm NumZ = EqlZ
--zeroComm (NumS a) = zeroComm a



timesLemma :: Natural a -> Natural b -> Equal n m -> Equal (a :+: (b :+: n)) (b :+: (a :+: m))
timesLemma a b z = (assocLemma a b lz) 
                    `transitive` (equality (plusCommutes a b) z) 
                    `transitive` (symmetric (assocLemma b a rz))
  where lz = z `transitive` (symmetric z)
        rz = (symmetric z) `transitive` z

-- This is the proof that the kata requires.
-- | a * b = b * a
timesComm :: Natural a -> Natural b -> Equal (a :*: b) (b :*: a)
timesComm NumZ NumZ = EqlZ
timesComm NumZ (NumS b) = timesComm NumZ b
timesComm (NumS a) NumZ = timesComm a NumZ
timesComm (NumS a) (NumS b) = symmetric $ EqlS ((increaseByA a (timesComm b (NumS a)))
                          `transitive` (timesLemma a b (timesComm a b)) 
                          `transitive` (increaseByA b (timesComm (NumS b) a)))
