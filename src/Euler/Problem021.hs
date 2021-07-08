module Euler.Problem021 (euler21, d)
where

import Data.Numbers.Primes (primeFactors)
import Data.List (subsequences, nub)
import Data.Char (digitToInt)

d :: Int -> Int
d = sum . init . nub . map product . subsequences . primeFactors

euler21 :: Int -> Int
euler21 n = sum [i | i <- [2..(n-1)], let a = d i, d a == i, i /= a]