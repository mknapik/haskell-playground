module Euler.Problem032 (euler32) where

import Data.List
import Data.Numbers.Primes
import Debug.Trace
import Data.Char (digitToInt)

euler32 n = filter ((==9) . length . nub . sort . concatMap show) inter
  where
    inter = nub $ sort $ map (\(a, b) -> sort [product a, product b, n]) $ f factors [] []

    f a b c | trace ("f " ++ show a ++ " " ++ show b ++ " " ++ show c) False = undefined
    f [] left right = [(left, right)]
    f [x] left right = [(x:left, right), (left, x:right)]
    f (x:xs) left right = f xs (x:left) right ++ f xs left (x:right)

    factors = primeFactors n