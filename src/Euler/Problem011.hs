module Euler.Problem011
where

import Data.List (transpose)

euler11 :: (Ord a, Num a) => Int -> [[a]] -> a
euler11 n grid = maximum $ map product result
  where
    size = length grid
    rows = concatMap (\row -> [(take n . drop x) row | x <- [0 .. size - n]])
    colt = rows (transpose grid)
    rowt = rows grid
    diag = \g -> [[g !! (x + i) !! (y + i) | i <- [0 .. n -1]] | x <- [0 .. size - n], y <- [0 .. size - n]]
    diag1 = diag grid
    diag2 = diag $ map reverse grid
    result = diag2 ++ diag1 ++ rowt ++ colt
    t = colt ++ rowt
    cells = [(x, y) | x <- [0 .. size -1], y <- [0 .. size -1]]
