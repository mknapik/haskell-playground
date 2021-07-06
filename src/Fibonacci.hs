module Fibonacci (fibs)
where

fibs :: Int -> [Int]
fibs 1 = [1]
fibs 2 = [2, 1]
fibs n =
  n2 + n1 : n2 : (n1 : tail)
  where
    (n2 : n1 : tail) = fibs (n - 1)