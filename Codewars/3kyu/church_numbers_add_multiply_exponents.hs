-- https://www.codewars.com/kata/55c0c452de0056d7d800004d
module Haskell.Codewars.Church where
--import Haskell.Codewars.Church.Preloaded -- Remove before submit

type Lambda a = (a -> a)
type Cnum a = Lambda a -> Lambda a

churchAdd :: Cnum a -> Cnum a -> Cnum a
churchAdd c1 c2 s = c1 s . c2 s

churchMul :: Cnum a -> Cnum a -> Cnum a
churchMul c1 c2 = c1 . c2

--Extra credit: Why is the type signature different?
churchPow :: Cnum a -> (Cnum a -> Cnum a) -> Cnum a
churchPow cb ce = ce cb
