module Euler.Problem005
where

import Data.Numbers.Primes (primeFactors)
import Data.List

euler05 :: Int -> Int
euler05 n = product $ helper n []
  where
    helper n acc
      | n == 1 = acc
      | otherwise = helper (n -1) (acc ++ (fs \\ acc))
      where
        fs = primeFactors n
