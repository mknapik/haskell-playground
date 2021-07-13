module Euler.Problem047 (euler47) where

import Data.List (nub, sort)
import Data.Numbers.Primes (primeFactors)

euler47 speedupStart n = (sort . head . filterNConsecutive . reduceConsecutiveFactors . filterNFactors) [speedupStart ..]
  where
    filterNConsecutive = filter ((== n) . length)
    filterNFactors = filter ((== n) . length . nub . primeFactors)
    reduceConsecutiveFactors = scanl reduce []
    reduce [] b = [b]
    reduce (a : t) b
      | b - a == 1 = b : a : t
      | otherwise = [b]
