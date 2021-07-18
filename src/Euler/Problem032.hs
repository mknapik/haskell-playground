module Euler.Problem032 (euler32) where

import Control.Monad
import Data.Char (digitToInt)
import Data.List
import Data.Numbers.Primes
import Data.Set (fromList, member)
import Debug.Trace

euler32 xs = ((: []) . sum . nub . sort . concatMap permsMulti) groups
  where
    groups = filter groupSizes $ generate xs [] [] []

generate :: [Int] -> [Int] -> [Int] -> [Int] -> [([Int], [Int], [Int])]
generate [] a b c = [(a, b, c)]
generate (x : t) a b c = generate t (x : a) b c ++ generate t a (x : b) c ++ generate t a b (x : c)
groupSizes (a, b, c) = la == 1 && lb == 4 || la == 2 && lb == 3
  where
    la = length a
    lb = length b

permsMulti (a, b, c) =
  [ p
    | i <- permutations a,
      let i' = digitsToNumber i,
      j <- permutations b,
      let j' = digitsToNumber j,
      let p = j' * i',
      let p' = reverse $ numberToDigits p,
      length p' == length c,
      null (p' \\ c)
  ]

digitsToNumber = foldl1 (\a b -> 10 * a + b)

numberToDigits :: Int -> [Int]
numberToDigits 0 = []
numberToDigits x = x `mod` 10 : numberToDigits (x `div` 10)