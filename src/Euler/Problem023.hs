module Euler.Problem023 (euler23) where

import Data.Char (digitToInt)
import Data.List (nub, subsequences)
import Data.Numbers.Primes (primeFactors)

d :: Int -> Int
d = sum . init . nub . map product . subsequences . primeFactors

abundant :: Int -> [Int]
abundant n = [i | i <- [1 .. n], let a = d i, a > i]

euler23 :: Int -> Int
euler23 n = sum [i | i <- [1 .. n], not $ test i]
  where
    a = abundant n
    test n = any (\x -> any (\y -> x + y == n) filtered) filtered'
      where
        filtered = filter (< n) a
        filtered' = reverse filtered

binsearch :: [Int] -> Int -> Int -> Int -> Int -- list, value, low, high, return int
binsearch xs value low high
  | high < low = -1
  | xs !! mid > value = binsearch xs value low (mid -1)
  | xs !! mid < value = binsearch xs value (mid + 1) high
  | otherwise = mid
  where
    mid = low + ((high - low) `div` 2)
