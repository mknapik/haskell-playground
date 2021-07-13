module Euler.Problem030 (euler30) where

import Data.Char (digitToInt)
import Data.Set (fromList, size)
import Debug.Trace

euler30 :: Int -> [Int]
euler30 k = [i | i <- [2 .. upperlimit], sumOfDigitPowers i == i]
  where
    upperlimit = findLimit 1

    findLimit :: Int -> Int
    findLimit n
      | s > m = findLimit (n + 1)
      | otherwise = s
      where
        m = (10 ^ n) - 1
        s = sumOfDigitPowers m

    sumOfDigitPowers :: Int -> Int
    sumOfDigitPowers = sum . map ((^ k) . digitToInt) . show
