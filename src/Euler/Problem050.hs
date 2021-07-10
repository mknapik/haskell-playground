module Euler.Problem050 (euler50) where

import Data.List (maximumBy)
import qualified Data.List as L
import Data.Numbers.Primes (isPrime, primes)
import Data.Ord (comparing)

euler50' :: Integral a => a -> [(a, Int)]
euler50' n =
  ( (: [])
      . maximumBy (comparing snd)
      . takeWhile ((< n) . fst)
      . filter (isPrime . fst)
      . map (\l -> (sum l, length l))
      . concat
      . scanl primeSequences []
      . takeWhile (< n)
  )
    primes
  where
    primeSequences prev p = [p] : [p : l | l <- prev]

euler50 :: Integral a => a -> [(a, Int)]
euler50 n =
  ( (: [])
      . maximumBy (comparing snd)
      . takeWhile ((< n) . fst)
      . filter (isPrime . fst)
      . concatMap (\a -> zip a [1 .. (length a)])
      . scanl primeSums [2]
      . drop 1
      . takeWhile (< n)
  )
    primes
  where
    primeSums prev p = p : [p + l | l <- prev, l < n]
