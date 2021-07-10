module Euler.Problem003 where

euler03 :: Int -> [Int]
euler03 n = euler03' n 2
  where
    bound = (floor . sqrt . fromIntegral) n
    original = n
    euler03' :: Int -> Int -> [Int]
    euler03' n k
      | n == 1 = []
      | n `mod` k == 0 = k : euler03' (n `div` k) k
      | k > bound = [original]
      | otherwise = euler03' n (k + 1)
