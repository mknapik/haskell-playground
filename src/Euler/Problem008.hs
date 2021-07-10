module Euler.Problem008 where

import Data.Char (digitToInt)

euler08 :: [Char] -> Int -> Int
euler08 n d = maximum ll
  where
    l = length n
    ll = [(product . map digitToInt . take d . drop skip) n | skip <- [0 .. l - d]]
