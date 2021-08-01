module Euler.Problem056 (euler56) where

import Data.Char (digitToInt)

euler56 as bs = (: []) $ maximum [s | a <- as, b <- bs, let n = a ^ b; s = (sum . map digitToInt . show) n]
