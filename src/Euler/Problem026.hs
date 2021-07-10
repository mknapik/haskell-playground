module Euler.Problem026 (euler26) where

import Data.List (maximumBy)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)

euler26 :: Int -> (Int, Int)
euler26 n =
  maximumBy
    (comparing snd)
    [ (i, size - 1 - index)
      | i <- [2 .. n],
        let fraction = remainders 1 i,
        let size = length fraction,
        let index = last fraction,
        index /= -1
    ]
  where
    remainders n d = remainders' Map.empty 0 n d
    remainders' :: Map.Map Int Int -> Int -> Int -> Int -> [Int]
    remainders' r c n d
      | n == 0 = [-1]
      | Map.member n r = [Map.findWithDefault (-1) n r]
      | otherwise = c : remainders' (Map.insert n c r) (c + 1) (10 * (n `mod` d)) d
