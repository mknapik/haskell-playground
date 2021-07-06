module Euler.Problem062 (euler62)
where
  
import Data.List
import Data.Numbers.Primes

-- The cube, 41063625 (345**3), can be permuted to produce two other cubes: 56623104 (384**3) and 66430125 (405**3). In fact, 41063625 is the smallest cube which has exactly three permutations of its digits which are also cube.

-- Find the smallest cube for which exactly five permutations of its digits are cube.
isCube :: Integer -> Bool
isCube x = round (fromIntegral x ** (1 / 3)) ^ 3 == x

-- euler62 :: [Integer]
euler62 =
  filter (\n -> length n == 5) $
    map
      ( filter isCube
          . nub
          . sort
          . map read
          . filter (\ns -> head ns /= '0')
          . permutations
          . show
      )
      cubes
  where
    cubes = [n * n * n | n <- [3 ..]]

