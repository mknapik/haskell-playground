module Euler.Problem042 (euler42) where

import Data.Char (digitToInt, ord)
import Data.List (elem, permutations)
import Math.NumberTheory.Powers.Squares (isSquare')

roundSqrt :: Int -> Int
roundSqrt x = floor (sqrt (fromIntegral x))

begin = ord 'A' - 1

euler42 :: String -> [Int]
euler42 = (: []) . length . filter (isTriangle . alphabetical) . words

alphabetical = sum . map (subtract begin . ord)

isTriangle tn
  | isSquare' delta = fn delta
  | otherwise = False
  where
    delta = 1 + 8 * tn
    fn delta = let s = roundSqrt delta in even (-1 - s) && even (-1 + s)
