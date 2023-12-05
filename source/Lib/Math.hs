module Lib.Math (fibs, primes) where

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

primes :: [Integer]
primes = sieve [2 ..]
  where
    sieve [] = []
    sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p /= 0]
