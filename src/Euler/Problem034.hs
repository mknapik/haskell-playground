module Euler.Problem034 (euler34) where

fact 0 = 1
fact 1 = 1
fact 2 = 2
fact 3 = 6
fact 4 = 24
fact 5 = 120
fact 6 = 720
fact 7 = 5040
fact 8 = 40320
fact 9 = 362880
fact n = product [1 .. n]

maxFact = fact 9

inBound i = i <= ((* maxFact) . numberLength) i

numberLength = length . show

-- bound = last $ takeWhile inBound [1..]
bound = 2540160

euler34 = [i | i <- [10 .. bound], let d = numberToDigits i, let p = sum $ map fact d, i == p]

numberToDigits :: Int -> [Int]
numberToDigits 0 = []
numberToDigits x = x `mod` 10 : numberToDigits (x `div` 10)
