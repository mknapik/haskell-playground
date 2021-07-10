module Changelog
  ( changelog,
  )
where

import Control.Applicative ((<$>))
import Data.Char (isSpace)
import Data.Function (on)
import Data.List (dropWhileEnd, findIndices, foldl, intercalate, isPrefixOf, sortBy)
import Data.Maybe (fromJust, isJust)
import System.IO (readFile)

trimr :: String -> String
trimr = dropWhileEnd isSpace

changelog = putStr =<< parse <$> readFile "CHANGELOG.md"

reduce prefix [] a = [[a]]
reduce prefix (h : t) a
  | p = [a] : h : t
  | otherwise = (a : h) : t
  where
    p = prefix `isPrefixOf` a

-- parse :: String
parse =
  intercalate "%\n"
    . map (intercalate "^\n")
    .
    -- map (map (intercalate "^^\n")) .
    -- map (map (foldl (reduce "### ") [])) .
    -- map (foldr (reduce "### ") []) .
    -- map (reverse) .
    -- reverse .
    foldl (reduce "## ") []
    . filter (not . null)
    . map trimr
    . lines
