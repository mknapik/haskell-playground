module Euler.Problem018
where

euler18 :: [[Int]] -> Int
euler18 rows = head $ foldr1 g rows
  where
    f x y z = x + max y z
    -- g xs ys | trace ("g " ++ show xs ++ " " ++ show ys) False = undefined
    g xs ys = zipWith3 f xs ys $ tail ys
