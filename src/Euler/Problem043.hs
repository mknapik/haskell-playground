module Euler.Problem043 (euler43) where

import Data.Char (digitToInt)
import Data.List (elem, permutations)

euler43 =
  ( (: [])
      . sum
      . map (foldl1 (\k l -> 10 * k + l))
      . filter f17
      . filter f13
      . filter f11
      . filter f7
      . filter f3
      . filter f2
      . filter f5
      . permutations
  )
    [0 .. 9]

f2 a = (a !! 3) `elem` [0, 2, 4, 6, 8]

f3 [] = False
f3 a = ((== 0) . (`mod` 3) . sum . take 3 . drop 2) a

f5 a = (a !! 5) `elem` [0, 5]

f :: Integral a => Int -> a -> [a] -> Bool
f idx n = (== 0) . (`mod` n) . foldl1 (\k l -> 10 * k + l) . take 3 . drop (idx - 1)

f7 = f 5 7

f11 = f 6 11

f13 = f 7 13

f17 = f 8 17
