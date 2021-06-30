# Description

This is a solution to the question [A*B=B*A? Prove it!](https://www.codewars.com/kata/5c302f562f6fe300155a1933)

# Explanation

First of all, if you are not familiar with the types here, please refer to my explanation in `addition_accosiativity` folder.

This is once again, proved by induction.

- if one of the number is zero (`Num Z`), then the result is is zero (`Z :*: m = Z`). (*My code here is a little bit over-complicated: one does not need the line 100 and 101 for induction, just replace line 99-101 with
```Haskell
timesComm NumZ _ = EqlZ
timesComm a NumZ = timesComm NumZ a
```
- To do the induction, we note the following peudo-code
```Haskell
b :*: (S a) == (S a) :*: b
<=> b :*: (S a) == b :+: (a :*: b) -- by definition
=> a :+: (b :*: (S a)) == a :+: (b :+: (a :*: b)) -- via  increaseBy
=> a :+: (b :+: (a :*: b)) == a :+: (b :*: (S a)) -- (1) via  symmetric

a :+: (b :+: (a :*: b)) == b :+: (a :+: (b :*: a)) -- (2) via  timesLema

-- the same procedure applied to b :*: a using  increaseBy  and  symmetric
b :+: (a :*: (S b)) == b :+: (a :+: (b :*: a)) -- (3)

-- combining (1), (2), and (3)
b :+: (a :*: (S b)) == a :+: (b :*: (S a))
<=> S (b :+: (a :*: (S b))) == S (a :+: (b :*: (S a)))
<=> (S b) :+: (a :*: (S b)) == (S a) :+: (b :*: (S a))
<=> (S a) :*: (S b) == (S b) :*: (S a)
-- which is what we want!
```
