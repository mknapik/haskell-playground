module Euler.Problem015
where

euler15 :: Num a => Int -> a
euler15 edges = last $ foldl1 (.) incF firstRow
  where
        vertices = edges + 1
        firstRow = replicate vertices 1
        incF = replicate edges (scanl1 (+))
