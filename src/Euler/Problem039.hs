module Euler.Problem039 (euler39) where

import Data.List (groupBy, maximumBy, sortBy)
import Data.Ord (comparing)

euler39 n = ((: []) . fst . head . mostCommonPerimeter . groupByPerimeter . sortByPerimeter) candidates
  where
    candidates = [(p, (a, b, c)) | a <- [1 .. n], b <- [a .. n], a + b < n, let c' = (sqrt . fromInteger) (a^2 + b^2); c = round c'; p = a + b + c, c' == fromInteger c, p <= n]
    sortByPerimeter = sortBy (comparing fst)
    mostCommonPerimeter = maximumBy (comparing length)
    groupByPerimeter = groupBy (\(a, _) (b, _) -> a == b)
    isInt x = x == fromInteger (round x)