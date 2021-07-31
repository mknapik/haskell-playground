module Euler.Problem052 (euler52) where

import Data.List (sort)
import Data.Numbers.Primes (isPrime, primes)

euler52 = (take 1 . filter same . ranges) [1 ..]

ranges = concatMap (\a -> [10 ^ (a -1) .. 10 ^ a `div` 6])

same :: (Show a, Num a) => a -> Bool
same number = all eq [2, 3, 4, 5, 6]
  where
    digits = (sort . show) number
    eq times = (sort . show) (times * number) == digits
