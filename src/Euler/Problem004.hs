module Euler.Problem004 (euler04) where

digs :: Int -> [Int]
digs 0 = []
digs x = x `mod` 10 : digs (x `div` 10)

euler04 :: [Int] -> Int
euler04 ns = maximum $ filter isNumberPalindrome [a * b | a <- ns, b <- ns]
  where
    isPalindrome :: [Int] -> Bool
    isPalindrome ns
      | l <= 1 = True
      | last ns == head ns = isPalindrome $ init tail
      | otherwise = False
      where
        l = length ns
        n : tail = ns
    isNumberPalindrome :: Int -> Bool
    isNumberPalindrome n = isPalindrome $ digs n
