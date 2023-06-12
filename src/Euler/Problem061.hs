module Euler.Problem061 (euler61) where

import Control.Monad (ap)
import Data.List ((\\), intersect, subsequences)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

(from, to) = (1000, 10000)
combinations k ns = filter ((k==).length) $ subsequences ns

-- euler61 = (takeWhile (<to) . dropWhile (<from)) $
-- euler61 = concatMap (\fn -> map fn [1..]) [triangle, square, pentagonal, hexagonal, heptagonal, octagonal]
euler61 = take 10 $ concatMap chain numbers
  where
    generators = [triangle, square, pentagonal, hexagonal, heptagonal, octagonal]

    numbers = concatMap (takeWhile (< to) . dropWhile (< from) . f) generators
    dups = concatMap (\[a, b] -> a `intersect` b) $ combinations 2 ll
    f fn = map fn [1 ..]
    g fn = (takeWhile (< to) . dropWhile (< from) . map fn) [1 ..]
    ll = [l3', l4', l5', l6', l7', l8']
    l3' = g triangle
    l4' = g square
    l5' = g pentagonal
    l6' = g hexagonal
    l7' = g heptagonal
    l8' = g octagonal
    l3 = l3' \\ dups
    l4 = l4' \\ dups
    l5 = l5' \\ dups
    l6 = l6' \\ dups
    l7 = l7' \\ dups
    l8 = l8' \\ dups
    s3 = S.fromList l3
    s4 = S.fromList l4
    s5 = S.fromList l5
    s6 = S.fromList l6
    s7 = S.fromList l7
    s8 = S.fromList l8
    listlist =
      [ (3, l3),
        (4, l4),
        (5, l5),
        (6, l6),
        (7, l7),
        (8, l8)
      ]
    next n = (n, next' numbers n)
    mapings = M.fromList $ map (\(idx, s) -> (idx, (M.fromList . filter (not . null . snd) . map next) s)) listlist

    next' [] number = []
    next' (h : tail) n
      | n /= h && n `mod` 100 == h `div` 100 = h : next' tail n
      | otherwise = next' tail n

    chain n = filter (\ss -> head ss `div` 100 == last ss `mod` 100) $ filter ((== 6) . length) $ chain' [3, 4, 5, 6, 7, 8] n
    chain' [] n = [[]]
    chain' tags n =
      [ n : t
        | tag <- tags,
          let maping = M.findWithDefault M.empty tag mapings,
          let found = M.findWithDefault [] n maping,
          f <- found,
          let tails = chain' (tags \\ [tag]) f,
          t <- tails
      ]

    ff chain = all (\s -> count (`S.member` s) chain == 1) [s3, s4, s5, s6, s7, s8]

count fn = length . filter fn

triangle n = n * (n + 1) `div` 2

square n = n ^ 2

pentagonal n = n * (3 * n - 1) `div` 2

hexagonal n = n * (2 * n - 1)

heptagonal n = n * (5 * n - 3) `div` 2

octagonal n = n * (3 * n - 2)
