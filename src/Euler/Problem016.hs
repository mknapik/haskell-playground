module Euler.Problem016 where

euler16 :: Int -> Int
euler16 n = sum $ map toInt $ show (2 ^ n)
  where
    toInt :: Char -> Int
    toInt = read . (: [])
