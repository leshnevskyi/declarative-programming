module Main (main) where

import Conduit
import Data.List (intersect, nub, sort, union, (\\))
import Lib.Data.List
  ( reversedQuickSort,
    uselessList10,
    uselessList10',
    uselessList10C,
    uselessList5,
    uselessList5',
    uselessList5C,
  )
import Lib.Math (primes)

firstList :: [Integer]
firstList = take 45 uselessList5

firstList' :: [Integer]
firstList' = take 45 uselessList5'

secondList :: [Integer]
secondList = take 60 uselessList10

secondList' :: [Integer]
secondList' = take 60 uselessList10'

main :: IO ()
main = do
  print $ take 1000 primes

  print $ reversedQuickSort firstList
  print $ sort firstList
  print $ nub firstList
  print $ firstList == firstList'
  print =<< runConduit (uselessList5C .| takeC 45 .| sinkList)

  print $ reversedQuickSort secondList
  print $ secondList == secondList'

  print $ firstList `union` secondList
  print $ firstList `intersect` secondList
  print $ firstList \\ secondList
  print =<< runConduit (uselessList10C .| takeC 60 .| sinkList)
