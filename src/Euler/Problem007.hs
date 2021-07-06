module Euler.Problem007
where

import Data.Numbers.Primes (primes)

euler07 :: Int -> Int
euler07 n = last $ take n primes
