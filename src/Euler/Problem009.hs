module Euler.Problem009 where

euler09 :: Int -> (Int, Int, Int)
euler09 n = head [(a, b, c) | a <- [1 .. n], b <- [a .. n - a], c <- [b .. n - a - b], a * a + b * b == c * c, a + b + c == n]
