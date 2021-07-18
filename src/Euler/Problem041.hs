module Euler.Problem041 (euler41) where

import Data.List (sort, permutations)
import Data.Numbers.Primes (primes, isPrime)
import Helper

euler41 :: [Int]

euler41 = ((:[]) . maximum . filter isPrime . map digitsToNumber . concatMap permutations . candidates . numberToDigits) 987654321

candidates [] = []
candidates (h:t) = (h:t) : candidates t
