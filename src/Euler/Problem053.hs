module Euler.Problem053 (euler53) where

import Data.List (sort)
import qualified Data.Map.Strict as M
import Data.Numbers.Primes (isPrime, primes)

bound = 1000000

euler53 ns = ((: []) . length . filter (> bound)) [combinations n k | n <- ns, k <- [1 .. n]]

factorial' n = product [1 .. n]
factorial = (map factorial' [0 ..] !!)

combinations n k = factorial n `div` (factorial k * factorial (n - k))


