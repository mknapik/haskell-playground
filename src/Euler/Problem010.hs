module Euler.Problem010
where

import Data.Numbers.Primes (primes)

euler10 :: Int -> Int
euler10 n = sum $ takeWhile (< n) primes
