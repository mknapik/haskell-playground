module Euler.Problem012 where

import Data.List (group, length)
import Data.Numbers.Primes (primeFactors)

euler12 stop = euler12' 1 0
  where
    nDivisors n = product $ map ((+ 1) . length) (group (primeFactors n))
    euler12' i acc
      | l > stop = triangle
      | otherwise = euler12' (i + 1) triangle
      where
        triangle = i + acc
        l = nDivisors triangle
