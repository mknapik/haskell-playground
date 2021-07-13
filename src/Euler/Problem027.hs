module Euler.Problem027 (euler27) where

import Data.List
import Data.Set (fromList, member)
import Data.Numbers.Primes (isPrime, primes)
import Data.Ord (comparing)

euler27 :: Integer -> [(Integer, Integer, Int)]
euler27 m =
  (: []) $
    maximumBy
      (comparing (\(a, b, p) -> p))
      [ (a, b, primes) | a <- [(- m + 1) .. (m -1)], b <- bs, let primes = length $ takeWhile (`member` p) (map (formula a b) [0 ..])
      ]
  where
    bs = takeWhile (<= m) primes
    p = fromList $ take 1000 primes
    formula :: Integer -> Integer -> Integer -> Integer
    formula a b n = n ^ 2 + a * n + b
