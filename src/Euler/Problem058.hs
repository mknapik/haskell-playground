module Euler.Problem058 (euler58) where

import Data.Numbers.Primes (isPrime)

euler58 = (take 1 . map fst . dropWhile cond . drop 1 . zip [1, 3 ..] . scanl1 agg . map (countPrimes . diag)) [1 ..]
  where
    agg (p1, t1) (p2, t2) = (p1 + p2, t1 + t2)
    cond (_, (a, b)) = fromIntegral a / fromIntegral b >= 0.10
    countPrimes ns = ((length . filter isPrime) ns, length ns)

layer 1 = [1 .. 1 ^ 2]
-- layer 2 = [1 ^ 2 + 1 .. 3 ^ 2]
-- layer 3 = [3 ^ 2 + 1 .. 5 ^ 2]
-- layer 4 = [5 ^ 2 + 1 .. 7 ^ 2]
layer n = [(2 * n - 3) ^ 2 + 1 .. ((2 * n) - 1) ^ 2]

diag 1 = [1]
-- diag 2 = [3, 5, 7, 9]
-- diag 3 = [13, 17, 21, 25]
-- diag 4 = [31, 37, 43, 49]
diag n' = [n2, n2 - n1, n2 - 2 * n1, n2 - 3 * n1]
  where
    n = (2 * n') - 1
    n2 = n ^ 2
    n1 = n - 1

-- 43 44 45 46 47 48 49
-- 42 21 22 23 24 25 26
-- 41 20  7  8  9 10 27
-- 40 19  6  1  2 11 28
-- 39 18  5  4  3 12 29
-- 38 17 16 15 14 13 30
-- 37 36 35 34 33 32 31

-- 1x1  3x3    5x5    7x7    9x9
-- 1    2      3      4      5
-- 1    3^2    5^2    7^2    9^2
-- 1    3^2-2  5^2-04 7^2-06 9^2-08
-- 1    3^2-4  5^2-08 7^2-12 9^2-16
-- 1    3^2-6  5^2-12 7^2-18 9^2-24
-- 1    n^2
-- 1    n^2-(n-1)
-- 1    n^2-2(n-1)
-- 1    n^2-3(n-1)
