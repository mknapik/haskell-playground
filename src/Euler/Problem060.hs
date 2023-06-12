module Euler.Problem060 (euler60) where

import Data.Numbers.Primes

euler60 =
  (map sum . take 1)
    [ [a, b, c, d, e]
      | a <- primes,
        b <- takeWhile (< a) primes,
        pairConcatPrimes b [a],
        c <- takeWhile (< b) primes,
        pairConcatPrimes c [a, b],
        d <- takeWhile (< c) primes,
        pairConcatPrimes d [a, b, c],
        e <- takeWhile (< d) primes,
        pairConcatPrimes e [a, b, c, d]
    ]
  where
    pairConcatPrimes a bs =
      all
        (isPrime . read)
        ( concatMap
            ((\b -> [a' ++ b, b ++ a']) . show)
            bs
        )
      where
        a' = show a
