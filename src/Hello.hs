module Hello
  ( hello,
  )
where

import Control.Monad
import Data.Char

hello = do
  line <- getLine
  when (not . null $ line) $ do
    putStrLn (reverseWords line)
    hello

reverseWords :: String -> String
reverseWords = unwords . map reverse . words
