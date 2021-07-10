module Euler.Problem019 (euler19) where

data Month = January | February | March | April | May | June | July | August | September | October | November | December deriving (Show, Eq)

data Days = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Show, Eq)

months = [January, February, March, April, May, June, July, August, September, October, November, December]

days = [Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday]

years from to = to - from + 1

euler19 :: Int
euler19 = sundays 1900 2000 - sundays 1900 1900

sundays from to =
  length $ filter (\(n, day) -> n == 1 && day == Sunday) $ zip alldays (cycle days)
  where
    m = take (years from to * length months) $ cycle months
    y = concatMap (replicate $ length months) [from .. to]
    my = zip m y
    leap year = year `mod` 4 == 0 && (year `mod` 400 == 0 || year `mod` 100 /= 0)
    alldays = concatMap ((\a -> [1 .. a]) . monthdays) my
    monthdays :: (Month, Int) -> Int
    monthdays (September, _) = 30
    monthdays (April, _) = 30
    monthdays (June, _) = 30
    monthdays (November, _) = 30
    monthdays (January, _) = 31
    monthdays (February, year) = if leap year then 29 else 28
    monthdays (_, _) = 31
