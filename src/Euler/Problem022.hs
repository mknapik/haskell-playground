module Euler.Problem022 (euler22) where

import Data.Char (digitToInt)
import Data.List (sort)
import Data.Numbers.Primes (primeFactors)
import Data.Text.Internal.Unsafe.Char (ord)

euler22 = (: []) . sum . zipWith (*) [1 ..] . map alphabetical . sort . words

begin = ord 'A' - 1

alphabetical = sum . map (subtract begin . ord)
