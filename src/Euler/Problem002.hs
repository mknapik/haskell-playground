module Euler.Problem002
where

import Fibonacci (fibs)

euler02 :: Int -> Int
euler02 cap = sum $ filter even $ takeWhile (< cap) (reverse (fibs 2000))
