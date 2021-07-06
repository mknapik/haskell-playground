module Euler.Problem013
where

euler13 :: (Show a, Foldable t, Num a) => t a -> [Char]
euler13 ns = take 10 $ show $ sum ns
