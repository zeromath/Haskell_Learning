# Description

This is a solution to the question [Evaluate mathematical expression](https://www.codewars.com/kata/52a78825cdfc2cfc87000005)


# Explanation

This is a standard way of using `Parsec` to parser an expression. See [Text.Parsec.Expr](https://hackage.haskell.org/package/parsec-free-3.1.11.5/docs/Text-Parsec-Expr.html)

We define the operator table in`table` with the desired operator priority. Then we feed this into the `buildExpressionParser` function.

__Note that `reservedOp` function needs a `lexer`. The original code does not work.__

Another caveat is that the original `unary` does not support nested unary operators, (e.g., `- -3` is not allowed). Hence we need the `nestedUnary` function -- it combines all these functions from the right (since prefix is left associative).

_This is my first time using `Parsec`. There are a lot of places that can be improved._
