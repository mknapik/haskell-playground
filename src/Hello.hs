module Hello (
    hello
)
where

import Data.Char
import Control.Monad

hello = do
    line <- getLine
    when (not . null $ line) $ do
        putStrLn (reverseWords line)
        hello

reverseWords :: String -> String
reverseWords = unwords . map reverse . words
