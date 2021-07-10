module Euler.Problem048 (euler48) where

euler48 n = sum [i ^ i | i <- [1 .. n]]
