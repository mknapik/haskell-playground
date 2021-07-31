module Euler.Problem017 where

import Data.List (isInfixOf)
import Data.Map.Strict (findWithDefault, fromList, lookup)

-- euler17 = map write
euler17 = (: []) . sum . map (length . write)

nums =
  fromList $
    zip [0 ..] ("" : words "one two three four five six seven eight nine ten eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen")
      ++ zip [20, 30 .. 90] (words "twenty thirty forty fifty sixty seventy eighty ninety")

write n
  | 1000 < n = "error"
  | 1 <= n && n <= 19 || n < 100 && n `mod` 10 == 0 = pick n
  | t > 0 = pick t ++ "thousand" ++ write hun
  | h > 0 = pick h ++ "hundred" ++ (if dec /= 0 then "and" else "") ++ write dec
  | d > 0 = pick (d * 10) ++ write sig
  | otherwise = "error"
  where
    pick n = findWithDefault "null" n nums
    (t, hun) = n `divMod` 1000
    (h, dec) = hun `divMod` 100
    (d, sig) = dec `divMod` 10
