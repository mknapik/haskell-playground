module Euler.Problem038 (euler38) where

import Data.Char (digitToInt, intToDigit)
import Data.List (sort, maximumBy)
import Data.Numbers.Primes (isPrime, primes)
import Data.Set (fromList, member)
import Helper
import Data.Ord (comparing)

trd (_, _, c) = c

euler38 =
  ((: []) . maximumBy (comparing trd))
    [ (i, n, read cp :: Integer)
      | n <- [2 .. 9],
        i <- [1 .. 9999],
        let cp = concatMap (show . (* i)) [1 .. n],
        sort cp == "123456789"
    ]
