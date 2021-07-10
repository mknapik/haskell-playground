module Euler.Problem024 (euler24, euler24') where

import Data.Char (digitToInt, intToDigit)
import Data.List (nub, permutations, sort, subsequences)
import Data.Numbers.Primes (primeFactors)

euler24 :: [Int] -> [String]
euler24 = perms . map intToDigit

euler24' :: [Int] -> [String]
euler24' = sort . permutations . map intToDigit

perms :: [a] -> [[a]]
perms [] = [[]]
perms [a] = [[a]]
perms (x : xs) = concatMap (\(c, rest) -> [c : r | r <- perms rest]) result
  where
    (_, _, result) = foldl reduce ([], xs, []) (x : xs)
    reduce :: ([a], [a], [(a, [a])]) -> a -> ([a], [a], [(a, [a])])
    reduce (before, after, acc) x = (newBefore, newAfter, acc ++ [(x, allExceptCurrent)])
      where
        newBefore = before ++ [x]
        newAfter = tail after
        allExceptCurrent = before ++ after
