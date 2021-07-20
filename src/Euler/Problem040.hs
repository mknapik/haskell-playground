module Euler.Problem040 (euler40) where

import Data.Char (digitToInt)
import Data.List (unfoldr)

maxn = 1000000

euler40 xs = (: []) $ product . map (digitToInt . transform) $ reduce 1 0 [] xs
  where
    transform (idx, int, relidx) = show int !! relidx

reduce _ _ acc [] = acc
reduce int total acc (seek : t)
  | total <= seek && seek <= total + intlen = reduce int total ((seek - 1, int, seek - 1 - total) : acc) t
  | otherwise = reduce (int + 1) (total + intlen) acc (seek : t)
  where
    n = show int
    intlen = length n