module Euler.Problem029 (euler29) where

import Data.Set (fromList, size)

euler29 :: Integer -> [Int]
euler29 n = ((: []) . size . fromList) [a ^ b | a <- [2 .. n], b <- [2 .. n]]

