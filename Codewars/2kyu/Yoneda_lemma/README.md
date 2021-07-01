# Description

This is a solution to the question [Yoneda lemma](https://www.codewars.com/kata/5af33bcdde4c7f94a90000b3)


# Explanation

The Yoneda Lemma is basically the following isomorphism `Nat(h_a, F) \cong F(a)`. This is accomplished by the following computation

## Covariant Version
### to direction
For any `varphi in Nat(h_a, F)`, if we evaluate on `a`, we get
```Haskell
varphi(a) :: h_a(a) -> F(a) = Hom(a, a) -> F(a)
```
There is a canonical element `id` in `Hom(a, a)`. Hence we can apply `varphi(a)(id)` to get the corresponding element in `F(a)`.

### from direction
For the converse direction, consider the following natural transformation induced by the element `x in F(a)`: given an object `b`, we have `Hom(a, b) -> F(b)` defined as follows

Given an element `f in Hom(a, b)`, we have `fmap f :: F a -> F b`. Then we can apply `fmap f` to the element `x` to obtain the result.

## Contravariant Version
### to direction
Just need to reverse the `id` function

### from direction
For the converse direction, consider the following natural transformation induced by the element `x in F(a)`: given an object `b`, we have `Hom(b, a) -> F(b)` defined as follows

Given an element `f in Hom(b, a)`, we have `fmap (getOp f) :: F a -> F b`. Then we can apply `fmap (getOp f)` to the element `x` to obtain the result.



# Equivalent forms
Here are some equivalent but point-free codes
```Haskell
to :: Functor f => Nat (Hom a) f -> f a
to = ($ id)

from :: Functor f => f a -> Nat (Hom a) f
from = flip fmap 


to' :: Contravariant f => Nat (CoHom a) f -> f a
to' = ($ Op id)

from' :: Contravariant f => f a -> Nat (CoHom a) f
from' = flip (contramap . getOp)
```
