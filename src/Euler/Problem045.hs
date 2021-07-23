{-# LANGUAGE TupleSections #-}

module Euler.Problem045 (euler45) where

import Data.List (minimumBy)
import Data.Ord (comparing)
import qualified Data.Set as Set
import Math.NumberTheory.Powers.Squares (isSquare')

euler45 =
  take 2 $ (filter isTriangle . filter isPentagonal . map hexagonal) [2 ..]

triangle n = n * (n + 1) `div` 2

pentagonal n = n * (3 * n - 1) `div` 2

hexagonal n = n * (2 * n - 1)

isTriangle :: Int -> Bool
isTriangle n
  | isSquare' s = even (roundSqrt s - 1)
  | otherwise = False
  where
    s = 8 * n + 1

isHexagonal :: Int -> Bool
isHexagonal n
  | isSquare' s = (roundSqrt s + 1) `mod` 4 == 0
  | otherwise = False
  where
    s = 8 * n + 1

isPentagonal :: Int -> Bool
isPentagonal n
  | isSquare' s = (roundSqrt s + 1) `mod` 6 == 0
  | otherwise = False
  where
    s = 24 * n + 1

roundSqrt :: Int -> Int
roundSqrt x = floor (sqrt (fromIntegral x))
