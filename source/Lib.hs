module Lib (stringifyBinaryOp, stringMul, isValidPercentage) where

stringifyBinaryOp :: (Show a) => (a -> a -> a) -> (a, a) -> String
stringifyBinaryOp op (x, y) = show (op x y)

stringMul :: (Int, Int) -> String
stringMul = stringifyBinaryOp (*)

inRangeValidator :: (Ord a) => a -> a -> (a -> Bool)
inRangeValidator start end = \value -> value >= start && value <= end

isValidPercentage :: Int -> Bool
isValidPercentage = inRangeValidator 0 100
