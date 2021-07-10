module Euler.Problem020 (euler20) where

import Data.Char (digitToInt)

euler20 n = sum $ map digitToInt $ show $ product [1 .. n]
