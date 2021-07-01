# Description

This is a solution to the question [Regular expression parser](https://www.codewars.com/kata/5470c635304c127cad000f0d)

# Explanation

This is an application of the `Megaparsec` library. See [Megaparsec tutorial
](https://markkarpov.com/tutorial/megaparsec.html) for a nice tutorial on this library.


The first two function are pretty straightforward

- `pNormalChar` eats any single character `c` that is not one of `()|.*` and returns `Normal c`.
- `pAny` eats a single character `.` and returns the `RegExp` type `Any`.

Then we have to be careful with `Or, ZeroOrMore, Str` as all of them eats more than one characters (not atomic).

The easiset one is `ZeroOrMore` -- it only occurs when we have (1) a single normal char (2) a dot (3) a parenthesis right before `*`. So we include those three cases in our function.

The slightly complicated one is `Or` --  it is of the form `(something)|(something)`, where `something` could be anything but `Or`.

The `Str` is basically the same idea -- it is `something` followed by `something` where `something` could be anything except `Str`.

Finally, we need to decide which parser goes first in `pExpr`. The `pOr` should have the highest priority. Then we try `pStr` and `pZeroOrMore`. After that, we can try the rest in any order. Note that all of `pOr, pStr, pZeroOrMore` will eat chars. Hence, we put `try` in front of each of them. Meanwhile, the rest (`pAny, pNormalChar, parens pExpr`) all start with very different characters (`pAny -> ., pNormalChar -> c, parens pExpr -> (..)`). Hence, we do not need `try`.
