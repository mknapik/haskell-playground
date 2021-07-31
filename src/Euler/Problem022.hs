module Euler.Problem022 (euler22) where

import Data.Char (digitToInt)
import Data.List (sort)
import Data.Text.Internal.Unsafe.Char (ord)

replace a b "" = ""
replace a b (x : xs)
  | a == x = b : replace a b xs
  | otherwise = x : replace a b xs

euler22 = (: []) . sum . zipWith (*) [1 ..] . map alphabetical . sort . words . replace ',' ' ' . replace '"' ' '

begin = ord 'A' - 1

alphabetical = sum . map (subtract begin . ord)
