module Euler.Problem005 where

import Data.List
import Data.Numbers.Primes (primeFactors)

euler05 :: Int -> Int
euler05 n = product $ helper n []
  where
    helper n acc
      | n == 1 = acc
      | otherwise = helper (n -1) (acc ++ (fs \\ acc))
      where
        fs = primeFactors n
