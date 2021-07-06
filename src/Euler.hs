module Euler
  ( euler01,
    euler02,
    euler03,
    euler04,
    euler05,
    euler06,
    euler07,
    euler08,
    euler09,
    euler10,
    euler11,
    euler12,
    euler13,
    euler14,
    euler15,
    euler16,
    euler18,
    collatzLength,
    lengths,
    euler60,
    euler62,
    euler760,
    fibs,
  )
where

import Debug.Trace
import Data.Bits
import Data.Char (digitToInt)
import Data.List
import qualified Data.Map.Strict as Map
import Data.Numbers.Primes
import qualified Data.Set as Set
import Flow

mod0 r n = n `mod` r == 0

sumf fs x = any (\f -> f x) fs

euler01 :: Int -> Int

euler01''' n = sum $ filter (sumf [mod0 3, mod0 5]) [1 .. n]

euler01'' n = sum $ Set.union (Set.fromList [0, 3 .. n]) (Set.fromList [0, 5 .. n])

euler01' n
  | n < 3 = 0
  | n `mod` 3 == 0 = n + euler01' (n -1)
  | n `mod` 5 == 0 = n + euler01' (n -1)
  | otherwise = euler01' (n -1)

euler01 n = euler01' (n -1)

fibs :: Int -> [Int]
fibs 1 = [1]
fibs 2 = [2, 1]
fibs n =
  n2 + n1 : n2 : (n1 : tail)
  where
    (n2 : n1 : tail) = fibs (n - 1)

euler02 :: Int -> Int
euler02 cap = sum $ filter even $ takeWhile (< cap) (reverse (fibs 2000))

euler03 :: Int -> [Int]
euler03 n = euler03' n 2
  where
    bound = (floor . sqrt . fromIntegral) n
    original = n
    euler03' :: Int -> Int -> [Int]
    euler03' n k
      | n == 1 = []
      | n `mod` k == 0 = k : euler03' (n `div` k) k
      | k > bound = [original]
      | otherwise = euler03' n (k + 1)

digs :: Int -> [Int]
digs 0 = []
digs x = x `mod` 10 : digs (x `div` 10)

euler04 :: [Int] -> Int
euler04 ns = maximum $ filter isNumberPalindrome [a * b | a <- ns, b <- ns]
  where
    isPalindrome :: [Int] -> Bool
    isPalindrome ns
      | l <= 1 = True
      | last ns == head ns = isPalindrome $ init tail
      | otherwise = False
      where
        l = length ns
        n : tail = ns
    isNumberPalindrome :: Int -> Bool
    isNumberPalindrome n = isPalindrome $ digs n

euler05 :: Int -> Int
euler05 n = product $ helper n []
  where
    helper n acc
      | n == 1 = acc
      | otherwise = helper (n -1) (acc ++ (fs \\ acc))
      where
        fs = primeFactors n

euler06 :: Int -> Int
euler06 n = a - b
  where
    a = ((^ 2) . sum) [1 .. n]
    b = (sum . map (^ 2)) [1 .. n]

euler07 :: Int -> Int
euler07 n = last $ take n primes

euler08 :: [Char] -> Int -> Int
euler08 n d = maximum ll
  where
    l = length n
    ll = [(product . map digitToInt . take d . drop skip) n | skip <- [0 .. l - d]]

euler09 :: Int -> (Int, Int, Int)
euler09 n = head [(a, b, c) | a <- [1 .. n], b <- [a .. n - a], c <- [b .. n - a - b], a * a + b * b == c * c, a + b + c == n]

euler10 :: Int -> Int
euler10 n = sum $ takeWhile (< n) primes

euler11 :: (Ord a, Num a) => Int -> [[a]] -> a
euler11 n grid = maximum $ map product result
  where
    size = length grid
    rows = concatMap (\row -> [(take n . drop x) row | x <- [0 .. size - n]])
    colt = rows (transpose grid)
    rowt = rows grid
    diag = \g -> [[g !! (x + i) !! (y + i) | i <- [0 .. n -1]] | x <- [0 .. size - n], y <- [0 .. size - n]]
    diag1 = diag grid
    diag2 = diag $ map reverse grid
    result = diag2 ++ diag1 ++ rowt ++ colt
    t = colt ++ rowt
    cells = [(x, y) | x <- [0 .. size -1], y <- [0 .. size -1]]

euler12 stop = euler12' 1 0
  where
    nDivisors n = product $ map ((+ 1) . length) (group (primeFactors n))
    euler12' i acc
      | l > stop = triangle
      | otherwise = euler12' (i + 1) triangle
      where
        triangle = i + acc
        l = nDivisors triangle

euler13 :: (Show a, Foldable t, Num a) => t a -> [Char]
euler13 ns = take 10 $ show $ sum ns

collatz :: Int -> Int
collatz n
  | even n = n `div` 2
  | otherwise = 3 * n + 1

collatzLength :: Int -> Map.Map Int Int -> (Int, Map.Map Int Int)
collatzLength 1 memo = (1, memo)
collatzLength n memo | Map.member n memo = (memo Map.! n, memo)
collatzLength n memo =
    let (len, memo') = collatzLength (collatz n) memo
     in (len + 1, Map.insert n (len + 1) memo')

lengths n = scanl next (1, 1, Map.empty) [2..n]
    where
      next (_, _, memo) n =
        let (len, memo') = collatzLength n memo
         in (len, n, memo')
sndOfThree (_, b, _) = b

euler14 n = (sndOfThree . maximum . lengths) n

euler15 :: Num a => Int -> a
euler15 edges = last $ foldl1 (.) incF firstRow
  where
        vertices = edges + 1
        firstRow = replicate vertices 1
        incF = replicate edges (scanl1 (+))

euler16 :: Int -> Int
euler16 n = sum $ map toInt $ show (2^n)
  where toInt :: Char -> Int
        toInt = read . (: [])

euler18 :: [[Int]] -> Int
euler18 rows = head $ foldr1 g rows
  where
    f x y z = x + max y z
    -- g xs ys | trace ("g " ++ show xs ++ " " ++ show ys) False = undefined
    g xs ys = zipWith3 f xs ys $ tail ys

euler60 :: Int
euler60 =
  (sum . head)
    [ [a, b, c, d, e]
      | a <- primes,
        b <- takeWhile (< a) primes,
        pairConcatPrimes b [a],
        c <- takeWhile (< b) primes,
        pairConcatPrimes c [a, b],
        d <- takeWhile (< c) primes,
        pairConcatPrimes d [a, b, c],
        e <- takeWhile (< d) primes,
        pairConcatPrimes e [a, b, c, d]
    ]
  where
    pairConcatPrimes a bs =
      all isPrime $
        map read $
          concatMap
            ((\b -> [a' ++ b, b ++ a']) . show)
            bs
      where
        a' = show a

-- The cube, 41063625 (345**3), can be permuted to produce two other cubes: 56623104 (384**3) and 66430125 (405**3). In fact, 41063625 is the smallest cube which has exactly three permutations of its digits which are also cube.

-- Find the smallest cube for which exactly five permutations of its digits are cube.
isCube :: Integer -> Bool
isCube x = round (fromIntegral x ** (1 / 3)) ^ 3 == x

-- euler62 :: [Integer]
euler62 =
  filter (\n -> length n == 5) $
    map
      ( filter isCube
          . nub
          . sort
          . map read
          . filter (\ns -> head ns /= '0')
          . permutations
          . show
      )
      cubes
  where
    cubes = [n * n * n | n <- [3 ..]]


euler760 :: Integer -> [Integer]
euler760 n = [g k (i-k) | i <- [0..n], k <- [0..i]]
  where g m n = (m `xor` n) + (m .|. n) + (m .&. n)
