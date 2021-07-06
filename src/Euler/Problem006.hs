module Euler.Problem006
where

euler06 :: Int -> Int
euler06 n = a - b
  where
    a = ((^ 2) . sum) [1 .. n]
    b = (sum . map (^ 2)) [1 .. n]
