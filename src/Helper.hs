module Helper where

import Data.Char (digitToInt)

numberToDigits :: Int -> [Int]
numberToDigits x = map digitToInt $ show x

digitsToNumber :: [Int] -> Int
digitsToNumber = foldl1 (\a b -> 10 * a + b)
