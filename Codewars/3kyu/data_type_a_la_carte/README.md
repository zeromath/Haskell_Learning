# Description

This is a solution to the question [Data Types à la Carte](https://www.codewars.com/kata/54808fc8ab03a23e82000a1f)


# Explanation

This is an implementation of the paper [Data types à la carte](https://doi.org/10.1017/S0956796808006758).

The paper basically deals with the question that how to add new data types without modifying existing codes. The key idea is to take the coproduct of these types.

The key idea of this question is the `Expr` wrapper, which wraps the data type and promotes it to a recursive verison. 

For example, the usual addition expression only involves
```Haskell
data Val e = Val Int
data Add e = Add e e
```
To evaluate them, we simply write
```Haskell
evalAlgebra (Val x) = x
evalAlgebra (Add x y) = x + y
```

Then the implemntation of `Expr` will take care of the recursion, combination and traversal.
