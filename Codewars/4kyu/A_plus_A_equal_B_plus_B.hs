-- https://www.codewars.com/kata/60ae5db1639df90055068d20
{-# LANGUAGE TypeOperators, TypeFamilies, GADTs, UndecidableInstances, TemplateHaskell #-}
module InvertAdd where

import InvertAddPreload

{- Imported code:
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
-}

-- lemmas
reflexive :: Natural n -> Equal n n
reflexive NumZ = EqlZ
reflexive (NumS n) = EqlS . reflexive $ n

symmetric :: Equal a b -> Equal b a
symmetric EqlZ = EqlZ
symmetric (EqlS x) = EqlS . symmetric $ x

transitive :: Equal a b -> Equal b c -> Equal a c
transitive EqlZ EqlZ = EqlZ
transitive (EqlS x) (EqlS y) = EqlS $ transitive x y

plusCommutes :: Natural a -> Natural b -> Equal (a :+: b) (b :+: a)
plusCommutes NumZ NumZ = EqlZ
plusCommutes NumZ (NumS b) = EqlS $ plusCommutes NumZ b
plusCommutes a NumZ = symmetric $ plusCommutes NumZ a
plusCommutes a (NumS b) = transitive (plusLemma a b) (EqlS $ plusCommutes a b)

plusLemma :: Natural a -> Natural b -> Equal (a :+: (S b)) (S (a :+: b))
plusLemma NumZ b = EqlS . reflexive $ b
plusLemma (NumS a) b = EqlS $ plusLemma a b

invert :: Natural a -> Natural b -> Equal (a :+: a) (b :+: b) -> Equal a b
invert NumZ NumZ EqlZ = EqlZ
invert (NumS n) (NumS m) (EqlS e1) = EqlS $ invert n m e2
  where
    EqlS e2 = plusCommutes (NumS n) n
              `transitive` e1
              `transitive` plusCommutes m (NumS m)
