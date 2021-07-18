module Euler.Problem033 (euler33) where

import Data.Ratio

euler33 =
  denominator $
    product
      [ a % c
        | a <- digits,
          b <- digits,
          a /= b,
          c <- digits,
          b /= c,
          let n = 10 * a + b,
          let d = 10 * b + c,
          n % d == a % c
      ]
  where
    digits = [1 .. 9]
