{-# LANGUAGE UnicodeSyntax #-}

module Lib
  ( someFunc,
  )
where

import qualified Data.Map as Map
import Flow

someFunc :: IO ()
-- someFunc = print [fib2 x | x <- [0..10]]
-- someFunc = print $ sort [3, 1, 5, 6, 2, 3]
-- someFunc = print [fizzbuzz x | x <- [0..30]]
-- someFunc = print $ elem 5 [0..100000000]
-- someFunc = print $ 3 :-: 4 :-: Empty .++ 4 :-: Empty
-- someFunc = print $ foldr treeInsert EmptyTree [1,3..100]
someFunc = print $ fmap' (+ 2) (Map.fromList [("asd", 2), ("qwe", 1)])

elem' :: (Eq a) => a -> [a] -> Bool
elem' y = foldl (\acc x -> x == y || acc) False

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 2) + fib (n - 1)

if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ y = y

collatz :: Int -> ([Int], Int)
collatz 1 = ([1], 0)
collatz n = (n : list, 1 + counter)
  where
    next = collatz $ if' (odd n) (3 * n + 1) (n `div` 2)
    list = fst next
    counter = snd next

collatz2 :: Int -> ([Int], Int)
collatz2 1 = ([1], 0)
collatz2 n =
  n
    |> \n ->
      if' (odd n) (3 * n + 1) (n `div` 2)
        |> collatz2
        |> \next -> (n : fst next, 1 + snd next)

fib2 :: Int -> [Int]
fib2 n = take n $ scanl (+) 0 (1 : [1 ..])

sort :: [Int] -> [Int]
sort [] = []
sort (x : xs) = small ++ x : big
  where
    small = [a | a <- xs, a <= x]
    big = [a | a <- xs, a > x]

fizzbuzz :: Int -> String
fizzbuzz n
  | mod n 15 == 0 = "FizzBuzz"
  | mod n 5 == 0 = "Buzz"
  | mod n 3 == 0 = "Fizz"
  | otherwise = show n

isin :: Eq t => t -> [t] -> Bool
isin t [] = False
isin t (h : list)
  | h == t = True
  | otherwise = isin t list

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
  let smallerSorted = quicksort [a | a <- xs, a <= x]
      biggerSorted = quicksort [a | a <- xs, a > x]
   in smallerSorted ++ [x] ++ biggerSorted

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (a : as) (b : bs) = f a b : zipWith' f as bs

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x : xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x : xs)
  | f x = x : filter' f xs
  | otherwise = filter' f xs

-- Let's find the largest number under 100,000
-- that's divisible by 3829.
-- To do that, we'll just filter a set of possibilities
-- in which we know the solution lies.
largestDivisible :: (Integral a) => a
largestDivisible = head $ filter (\n -> n `mod` 3829 == 0) [100000, 99999 ..]

-- Next up, we're going to find the sum of all odd squares
-- that are smaller than 10,000.
oddSquares :: (Integral a) => a
-- oddSquares = sum $ takeWhile (<=10000) [n*n | n <-[0, 1..], odd (n*n)]
oddSquares = sum $ takeWhile (<= 10000) $ filter odd $ map (^ 2) [1 ..]

infixr 5 :-:

data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

infixr 5 .++

(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert a EmptyTree = Node a EmptyTree EmptyTree
treeInsert x (Node a left right)
  | x == a = Node x left right
  | x < a = Node a (treeInsert x left) right
  | x > a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
  | x == a = True
  | x < a = treeElem x left
  | x > a = treeElem x right

class YesNo a where
  yesno :: a -> Bool

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo [a] where
  yesno [] = False
  yesno _ = True

instance YesNo Bool where
  yesno = id

instance YesNo (Maybe a) where
  yesno (Just _) = True
  yesno Nothing = False

yay :: (YesNo y) => y -> a -> a -> a
yay y yes no = if yesno y then yes else no

class Functor' f where
  fmap' :: (a -> b) -> f a -> f b

instance Functor' [] where
  fmap' f [] = []
  fmap' f (head : tail) = f head : fmap' f tail

instance (Ord k) => Functor' (Map.Map k) where
  fmap' f m = Map.fromList $ fmap' (\(k, v) -> (k, f v)) (Map.toList m)

instance Functor' Maybe where
  fmap' f Nothing = Nothing
  fmap' f (Just v) = Just $ f v
