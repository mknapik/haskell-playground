module Euler.Problem059 (euler59) where

import Data.Bits (xor)
import Data.Char (chr, ord)
import Data.List (isInfixOf)

replace a b "" = ""
replace a b (x : xs)
  | a == x = b : replace a b xs
  | otherwise = x : replace a b xs

-- euler59 input = (map (sum . map ord) . (: []) . maximumBy (compare `on` ((length .) . filter . (==)) 'e')) candidates
-- euler59 input = (map (sum . map ord) . filter commonWord) candidates
euler59 = map (sum . map ord) . filter commonWord . candidates . parseCipher
  where
    alphabet = ['a' .. 'z']
    keys = [map ord [a, b, c] | a <- alphabet, b <- alphabet, c <- alphabet]

    parseCipher :: [Char] -> [Int]
    parseCipher = map read . words . replace ',' ' '

    crack :: [Int] -> [Int] -> [Char]
    crack = (map chr .) . zipWith xor . cycle

    candidates :: [Int] -> [[Char]]
    candidates = flip map keys . flip crack

    commonWord :: [Char] -> Bool
    commonWord text = " the " `isInfixOf` text
