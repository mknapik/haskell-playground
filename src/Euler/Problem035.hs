module Euler.Problem035 (euler35) where

import Data.Char (digitToInt)
import Data.List (permutations)
import Data.Numbers.Primes

euler35 n = ((: []) . length . filter circular . takeWhile (< n)) primes

circular = all (isPrime . digitsToNumber) . rotations . numberToDigits

digitsToNumber = foldl1 (\a b -> 10 * a + b)

numberToDigits :: Int -> [Int]
numberToDigits x = map digitToInt $ show x

rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs n = (take (length xs) . drop n . cycle) xs

rotations :: [Int] -> [[Int]]
rotations xs = map (rotate xs) [1 .. (length xs)]