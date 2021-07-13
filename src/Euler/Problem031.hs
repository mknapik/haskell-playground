module Euler.Problem031 (euler31) where

import Data.List (group)
coins = reverse [1, 2, 5, 10, 20, 50, 100, 200]

euler31 :: Int -> Int
euler31 n = test n coins

test :: Int -> [Int] -> Int
test n [] = 0
test 0 _ = 1
test n (h : t)
  | h > n = test n t
  | otherwise = test (n - h) (h : t) + test n t

-- test :: Int -> [Int] -> Int
-- test n [] = []
-- test 0 (h:_) = [[]]
-- test n (h : t)
--   | h > n = test n t
--   | otherwise = map (h :) (test (n - h) (h : t)) ++ test n t
