module Euler.Problem046 (euler46) where

import Data.Numbers.Primes (isPrime, primes)

max0 = 10000

euler46' =
  take 1 $ dropWhile test $ filter (not . isPrime) [3, 5 ..]
  where
    p = takeWhile (< max0) primes
    s = takeWhile (< max0) doubleSquares
    test n = any (\p -> any (\s -> s + p == n) (takeWhile (<= (n - p)) s)) $ takeWhile (< n) primes

doubleSquares = map ((* 2) . (^ 2)) [1 ..]

euler46 =
  (take 1 . dropWhile test . filter (not . isPrime)) [3, 5 ..]
  where
    test n = any (\s -> isPrime (n - s)) (takeWhile (<= n) doubleSquares)
