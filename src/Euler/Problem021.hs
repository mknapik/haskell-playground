module Euler.Problem021 (euler21, d) where

import Data.Char (digitToInt)
import Data.List (nub, subsequences)
import Data.Numbers.Primes (primeFactors)

d :: Int -> Int
d = sum . init . nub . map product . subsequences . primeFactors

euler21 :: Int -> Int
euler21 n = sum [i | i <- [2 .. (n -1)], let a = d i, d a == i, i /= a]
