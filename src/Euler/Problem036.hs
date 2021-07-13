module Euler.Problem036 (euler36) where

import Data.Char (intToDigit)
import Data.List (unfoldr)
import Numeric (showIntAtBase)

showBinary :: Int -> [Char]
showBinary x =
  reverse $ binstr $ unfoldr ndiv x
  where
    binstr = map ("01" !!)
    exch (a, b) = (b, a)
    ndiv n =
      case n of
        0 -> Nothing
        _ -> Just $ exch $ n `divMod` 2

isPalindrome :: Eq a => [a] -> Bool
isPalindrome s = s == reverse s

euler36 n = (: []) $ sum $ filter (isPalindrome . showBinary) $ filter (isPalindrome . show) [1 .. n]
