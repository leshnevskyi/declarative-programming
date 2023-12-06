module Lib.Data.List
  ( Lib.Data.List.product,
    product',
    product'',
    quickSort,
    reversedQuickSort,
    uselessList5,
    uselessList5',
    uselessList5C,
    uselessList10,
    uselessList10',
    uselessList10C,
  )
where

import Conduit
import Data.Function ((&))

product :: (Num a) => [a] -> a
product = foldr (*) 1

product' :: (Num a) => [a] -> a
product' [] = 1
product' (x : xs) = x * product' xs

product'' :: (Num a) => [a] -> a
product'' = product''' 1
  where
    product''' acc [] = acc
    product''' acc (x : xs) = product''' (x * acc) xs

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x : xs) =
  quickSort (filter (< x) xs) ++ [x] ++ quickSort (filter (>= x) xs)

reversedQuickSort :: (Ord a) => [a] -> [a]
reversedQuickSort = reverse . quickSort

endsWithDigit :: Integer -> Integer -> Bool
endsWithDigit number = (number `mod` 10 ==)

-- The task suggests to check if a number ends with 3, but the program hangs
-- because there are no numbers that end with 3 in the list.
uselessList5 :: [Integer]
uselessList5 = [1 ..] & filter (`endsWithDigit` 6) . map (^ 4)

uselessList5' :: [Integer]
uselessList5' = [x ^ 4 | x <- [1 ..], (x ^ 4) `endsWithDigit` 6]

uselessList5C :: (Monad m) => ConduitT () Integer m ()
uselessList5C = yieldMany [1 ..] .| mapC (^ 4) .| filterC (`endsWithDigit` 6)

uselessList10 :: [Integer]
uselessList10 = filter (== 3) $ map (`mod` 5) [1 ..]

uselessList10' :: [Integer]
uselessList10' = [x `mod` 5 | x <- [1 ..], x `mod` 5 == 3]

uselessList10C :: (Monad m) => ConduitT () Integer m ()
uselessList10C = yieldMany [1 ..] .| mapC (`mod` 5) .| filterC (== 3)
