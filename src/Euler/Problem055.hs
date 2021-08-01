module Euler.Problem055 (euler55) where

euler55 :: Show a => [a] -> [Int]
euler55 = (:[]) . length . filter isLychrel . map show

isLychrel = isLychrel' 51

isLychrel' 0 n = True
isLychrel' c n
  | isPalindrome candidate = False
  | otherwise = isLychrel' (c-1) candidate
  where
    n' = reverse n
    candidate = show $ read n + read n'


isPalindrome n = n == reverse n
