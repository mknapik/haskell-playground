module Euler.Problem033 (euler33) where

import Data.List
import Data.List (intersect)
import Debug.Trace

euler33 =
  (: []) $
    (\(a, b) -> let g = gcd a b in (a `div` g, b `div` g)) $
      foldl1 (\(n1, d1) (n2, d2) -> (n1 * n2, d1 * d2)) $
        [ (n, d)
          | d <- [10 .. 99],
            n <- [10 .. (d - 1)],
            n `mod` 10 /= 0,
            d `mod` 10 /= 0,
            let ns = numberToDigits n,
            let ds = numberToDigits d,
            let i = nub $ ns `intersect` ds,
            (not . null) i,
            any (test ns ds n d) i
        ]

test :: [Int] -> [Int] -> Int -> Int -> Int -> Bool
test ns ds n d i = n'' == n''' && d'' == d'''
  where
    n' = digitsToNumber $ delete i ns
    d' = digitsToNumber $ delete i ds
    gcd1 = gcd n d
    gcd2 = gcd n' d'
    n'' = n `div` gcd1
    d'' = d `div` gcd1
    n''' = n' `div` gcd2
    d''' = d' `div` gcd2

digitsToNumber :: [Int] -> Int
digitsToNumber = foldl1 (\a b -> 10 * a + b)

numberToDigits :: Int -> [Int]
numberToDigits = reverse . numberToDigits'

numberToDigits' 0 = []
numberToDigits' x = x `mod` 10 : numberToDigits' (x `div` 10)