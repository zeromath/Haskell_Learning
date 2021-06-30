-- https://www.codewars.com/kata/5672682212c8ecf83e000050
module Codewars.G964.DblLinear where 

type IncData = (Int, Int, [Int])

dblLinear :: Int -> Integer
dblLinear = (inc_data !!)
  where inc_data = 1: merge xs ys
        xs = map ((1+).(2*)) inc_data
        ys = map ((1+).(3*)) inc_data
        merge l1@(h1:t1) l2@(h2:t2)
          | h1 < h2 = h1: merge t1 l2
          | h1 > h2 = h2: merge l1 t2
          | otherwise = h1: merge t1 t2
