module Euler.Problem025 (euler25) where

import Data.Char (digitToInt, intToDigit)
import Data.List (nub, permutations, sort, subsequences)
import Data.Numbers.Primes (primeFactors)

numDigits :: Integer -> Integer
numDigits 0 = 1
numDigits n = toInteger $ floor (logBase 10 (fromIntegral n)) + 1


-- euler25 :: Integer -> Integer
euler25 n = take 1 $ map third $ dropWhile ((<n) . numDigits . first) $ scanl fs (0, 1, 0) [1 ..]
    where fs (k, l, _) n = (l, k + l, n)

first (a, _, _) = a
third (_, _, a) = a