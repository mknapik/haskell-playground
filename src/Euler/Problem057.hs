module Euler.Problem057 (euler57) where

import Data.NumberLength (numberLength)
import Data.Numbers.Primes (isPrime, primes)

g' 1 = (3, 2)
g' n = let (c, b) = g' (n - 1) in (c + 2 * b, c + b)

g = (map g' [1 ..] !!)

g'' _ 1 = (3, 2)
g'' (c, b) n = (c + 2 * b, c + b)

euler57 = (: []) . length . filter find . scanl g'' (0, 0)
  where
    find :: (Integer, Integer) -> Bool
    find (a, b) = len a > len b
      where
        len = numberLength
