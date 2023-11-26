module Lib.Data.List
  ( Lib.Data.List.product,
    product',
    product'',
    quickSort,
  )
where

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
