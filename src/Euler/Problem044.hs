{-# LANGUAGE TupleSections #-}

module Euler.Problem044 (euler44) where

import Data.List (minimumBy)
import Data.Ord (comparing)
import qualified Data.Set as Set
import Math.NumberTheory.Powers.Squares (isSquare')

-- 140-300ms
euler44' =
  (: []) $ head $ filter (\(a, b) -> Set.member (a + b) set) candidates
  where
    (candidates, set, serie) = scanl next ([], Set.empty, []) pseries !! 3000
    next (result, set, serie) n =
      (candidates ++ result, newset, n : serie)
      where
        candidates = map (n,) $ filter find serie
        find k = Set.member (n - k) set
        newset = Set.insert n set

-- 46ms
euler44 =
  candidates
  where
    (candidates, _) = head $ dropWhile (null . fst) $ scanl next ([], []) pseries
    next (_, serie) n =
      (candidates, n : serie)
      where
        candidates = (map (n,) . filter find_d . filter find_s) serie
        find_s k = isPentagonal (n + k)
        find_d k = isPentagonal (n - k)

pseries = map pentagonal [1 ..]

pentagonal n = n * (3 * n - 1) `div` 2

isPentagonal :: Int -> Bool
isPentagonal n
  | isSquare' s = (roundSqrt s + 1) `mod` 6 == 0
  | otherwise = False
  where
    s = 24 * n + 1

roundSqrt :: Int -> Int
roundSqrt x = floor (sqrt (fromIntegral x))
