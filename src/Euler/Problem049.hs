module Euler.Problem049 (euler49) where

import Control.Monad (ap)
import Data.Char (intToDigit)
import Data.Eq ()
import Data.List (groupBy, maximumBy, nub, sort, sortBy, subsequences)
import Data.Numbers.Primes (isPrime, primes)
import Data.Ord (comparing)

from = 1000

to = 10000

seqlen = 3

euler49 =
  ( --
    filter (all (== True) . pairwiseWith (==) . pairwiseWith (flip (-)))
      . concatMap (combinations seqlen)
      . filter ((>= seqlen) . length)
      . map (map fst)
      . groupBy ((. snd) . (==) . snd)
      . sortBy (comparing snd)
      . map (ap (,) sortedInt)
  )
    candidates
  where
    candidates = takeWhile (< to) $ dropWhile (< from) primes
    sortedInt = sort . show

euler49' =
  ( --
    filter (all (== True) . pairwiseWith (==) . pairwiseWith (flip (-)))
      . concatMap (combinations seqlen)
      . filter ((>= seqlen) . length)
      . map (map fst)
      . groupBy (\a b -> snd a == snd b)
      . sortBy (comparing snd)
      . map (\p -> (p, sortedInt p))
  )
    candidates
  where
    candidates = takeWhile (< to) $ dropWhile (< from) primes
    sortedInt = sort . show

pairwiseWith f l = zipWith f l (tail l)

combinations k ns = filter ((k ==) . length) $ subsequences ns

-- combinations = (. subsequences) . filter . (. length) . (==)
