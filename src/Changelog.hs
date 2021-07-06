module Changelog (
    changelog
)
where

import System.IO (readFile)
import Control.Applicative ((<$>))
import Data.Maybe (isJust, fromJust)
import Data.List (sortBy, dropWhileEnd, isPrefixOf, findIndices, foldl, intercalate)
import Data.Function (on)
import Data.Char (isSpace)

trimr :: String -> String
trimr = dropWhileEnd isSpace
changelog = putStr =<< parse <$> readFile "CHANGELOG.md"


reduce prefix [] a = [[a]]
reduce prefix (h:t) a
    | p = [a] : h : t
    | otherwise = (a:h) : t
    where p = prefix `isPrefixOf` a

-- parse :: String 
parse =
    intercalate "%\n" .
    map (intercalate "^\n") .
    -- map (map (intercalate "^^\n")) .
    -- map (map (foldl (reduce "### ") [])) .
    -- map (foldr (reduce "### ") []) .
    -- map (reverse) .
    -- reverse .
    foldl (reduce "## ") []  .
    filter (not . null) . 
    map trimr . 
    lines
