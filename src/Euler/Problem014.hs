module Euler.Problem014
where

import qualified Data.Map.Strict as Map

collatz :: Int -> Int
collatz n
  | even n = n `div` 2
  | otherwise = 3 * n + 1

collatzLength :: Int -> Map.Map Int Int -> (Int, Map.Map Int Int)
collatzLength 1 memo = (1, memo)
collatzLength n memo | Map.member n memo = (memo Map.! n, memo)
collatzLength n memo =
    let (len, memo') = collatzLength (collatz n) memo
     in (len + 1, Map.insert n (len + 1) memo')

lengths n = scanl next (1, 1, Map.empty) [2..n]
    where
      next (_, _, memo) n =
        let (len, memo') = collatzLength n memo
         in (len, n, memo')
sndOfThree (_, b, _) = b

euler14 :: Int -> Int
euler14 n = (sndOfThree . maximum . lengths) n
