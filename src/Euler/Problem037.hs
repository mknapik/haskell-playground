module Euler.Problem037 (euler37) where

import Data.Char (digitToInt)
import Data.Numbers.Primes (isPrime, primes)
import Helper

euler37 :: [Int]
euler37 = ((: []) . sum . take 11 . filter isTruncatable) primes

isTruncatable n
  | n < 10 = False
  | otherwise = isRightTruncatable n && isLeftTruncatable n

isLeftTruncatable n
  | n < 10 = isPrime n
  | isPrime n = (isLeftTruncatable . digitsToNumber . tail . numberToDigits) n
  | otherwise = False

isRightTruncatable n
  | n < 10 = isPrime n
  | isPrime n = isRightTruncatable (n `div` 10)
  | otherwise = False