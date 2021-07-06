# Description

This is a solution to the question [Operator Parser](https://www.codewars.com/kata/5842777813ee50ef38000020)


# Explanation

Let me carefully explain the procedure here. Let's start with the most basic case -- consider operations involving only addition `+` and multiplication `*`. Suppose that we are paring the expression `2 + 3 * 5 + 7`. 

1. First break it using the operator with least priority, i.e., the addition `+`.
2. Get `2` add `3 * 5` add `7`. Then we break each expression further (if possible) using the operator with the next priority. In thise case, we get `2` add (`3` mul `5`) add `7`.

Hence, the general idea is to break the expression into `term` op `term` op `term` using operators with the least priority, and then break each `term` into smaller terms using operators with the next level priority.

The key function to parse an expresssion is to use the `chainl1` function (or `chainr1` function if it is right associative). If one looks at the source code of `chainl1`, one will see that this function literally does what we described above -- parse the expression into "`term` op `term` op `term` ...".

The only problematic part here is the `NoAssociativity` operators. That's why we write `chainMiddle` function, which is basically a modification of `chainl1`, except that instead of "chaining" these operators, it just parse once.

Let's assume for simplicity that our operator table is the following
```Haskell
 [ 
 NoAssociativity "< > =", 
 L "+ -", 
 L "* /"
 ]
```
So our parser is basically (in pseudo-code)
```Haskell
terms = chainMiddle term1 (optr "<" +++ optr ">" +++ optr "=")
  where
    term1 = chainl1 term2 (optr "+" +++ optr "-")
      where
	    term2 = chainr1 term3 (optr "*" +++ optr "/")
	      where
		    term3 = OneSingleTerm +++ parens (terms)
```
Note that in `Text.ParserCombinators.ReadP`, the equivalence of `<|>` is `+++`.


The rest of the code is fairly easy to understand

- `promoteOp` makes a `ReadP a` type into `ReadP (OpTree ...)` type
- `makeTermP` makes a `ReadP b` type into `ReadP (OpTree a b)` type
- `chainOp` deals with connecting each operator using `+++` and applies the correct chain function on them
- `parseOperators` nests these chained operators. Since operators with highest priority is the rightmost ones, we use the `foldr` function.
