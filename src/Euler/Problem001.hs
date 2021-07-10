module Euler.Problem001 (euler01) where

import qualified Data.Set as Set

mod0 r n = n `mod` r == 0

sumf fs x = any (\f -> f x) fs

euler01 :: Int -> Int

euler01''' n = sum $ filter (sumf [mod0 3, mod0 5]) [1 .. n]

euler01'' n = sum $ Set.union (Set.fromList [0, 3 .. n]) (Set.fromList [0, 5 .. n])

euler01' n
  | n < 3 = 0
  | n `mod` 3 == 0 = n + euler01' (n -1)
  | n `mod` 5 == 0 = n + euler01' (n -1)
  | otherwise = euler01' (n -1)

euler01 n = euler01' (n -1)
