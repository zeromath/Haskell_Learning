# Description

This is a solution to the question [A+B=B+A? Prove it!](https://www.codewars.com/kata/59db393bc1596bd2b700007f)


# Explanation

First of all, all natural numbers here are constructed using types.

We start with two basics: the type zero (`Z`) and the "function" `S` which takes a type `n` and returns a new type `S n`. One should think of `S n` as the successor of `n`. Then the natural numbers are the collection of types `Z, S Z, S (S Z), ...`. The type `Natural` contains two "functions", the constant function `NumZ`, which should be thought of as the number zero, and the successor function `NumS`. Hence, whenever we write `Natural a`, it must be either `NumZ` of `NumS b` for some `b`.

The tricky part is that the equality is also a type: `Equal` with two members -- one is that `EqlZ` (`Z == Z`) and the second is an induction, i.e., if `a == b`, then `S a == S b`, denote by `NumS $ Equal a b`.


Before giving the proof, we notice that from the Peano definition, we have `m :+: Z = m` by repeatedly apply those two instances.

The proof is fairly easy: 

- first, we have `NumZ + NumZ = NumZ = NumZ + NumZ`, i.e., `EqlZ`.
- we apply the induction on the case `NumZ + (NumS b)`: since we have `plusCommutes NumZ b`, i.e., `Equal (Z :+: b) (b :+: Z) == Equal b b`, applying `EqlS`, we get `Equal (S b) (S b)`, which is `Equal (Z :+: (S b)) ((S b) :+: Z)`.
- for the case `(NumS b) + NumZ`, we make use of the symmetry of `Equal a b` (proved in `symmetric`).
- for the general case `a + (NumS b)`, we first note that inductively we have `a + b`. So we already have `Equal (a :+: b) (b :+: a)` and we want to show `Equal (a :+: (S b)) ((S b) :+: a)`. By the instance (axiom), we have `Equal ((S b) :+: a) (S (b :+: a))`. By applying `EqlS`, we have `Equal (S (a :+: b)) (S (b :+: a))`. Hence, all we need to show is that `Equal (a :+: (S b)) (S (a :+: b))`. This is proved in the `plusLemma`. Here, I've been repeatedly using the transitivity (proved in `transitive`) of `Equal a b`.
