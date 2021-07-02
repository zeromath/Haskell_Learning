# Description

This is a solution to the question [Writing applicative parsers from scratch](https://www.codewars.com/kata/54f1fdb7f29358dd1f00015d)


# Explanation

The majority part of the introduction is quite clear. One can finish this kata just by reading through the introduction.

Here is a record of my struggling parts:

First of all, the error state is basically the empty result. The result of a parser `Parser a` is of the type `[(String, a)]`. Hence, if there is an error, we should return `[]`. (<- This was something that bothers me a lot when I was finishing this kata.)

> `predP` takes a predicate and will parse a single character, if the input contains at least one character and the predicate returns `True` applied to the character.

I was really confused here at my first glance -- I was not sure about the sentence "the input contains at least one character." Later, I knew that it means if the first char of the input string is "predicted" to be `True`, then parse that char, and don't do anything otherwise

Because of my misunderstanding, my `charP` function is a mess: this function `charP c` is just `predP` applied to the function `(==c)`. So the codes there could be greatly improved.

The `some` and `many` function can be greatly improved -- one can basically refer to the documents of `some, many` in `Control.Applicative` (See <https://hackage.haskell.org/package/base-4.15.0.0/docs/Control-Applicative.html>) and translate everything using the following cheat sheat:

```
<#>  is  <$>
<#   is  <$
<@>  is  <*>
<@   is  <*
 @>  is   *>
<<>> is  <|>
```
