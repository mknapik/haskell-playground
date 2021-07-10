module Euler.Problem025 (euler25) where

import Data.Char (digitToInt, intToDigit)
import Data.List (nub, permutations, sort, subsequences)
import Data.Numbers.Primes (primeFactors)

fibs2 = 0:1:zipWith (+) fibs2 (tail fibs2)

euler25 :: Integer -> [Integer]
euler25 n = take 1 $ map third $ dropWhile ((<(10^(n-1))) . first) $ scanl fs (0, 1, 0) [1 ..]
    where fs (k, l, _) n = (l, k + l, n)

first (a, _, _) = a
third (_, _, a) = a