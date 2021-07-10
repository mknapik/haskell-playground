module Euler.Problem028 (euler28) where

import Data.List (maximumBy)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)

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
-- 1    n^2-6*(n-1)

euler28 :: Int -> Int
euler28 diag = sum [test d | d <- [1, 3 .. diag]]
  where
    test 1 = 1
    test diag = 4 * diag ^ 2 - 6 * (diag - 1)
