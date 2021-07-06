module Main where

import Test.Hspec
import EulerSpec

main :: IO ()
main = hspec EulerSpec.spec
