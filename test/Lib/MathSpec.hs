module Lib.MathSpec (spec) where

import Lib.Math
import Test.Hspec

spec :: Spec
spec = do
  describe "fibs" $ do
    it "returns the correct Fibonacci sequence" $ do
      take 10 fibs `shouldBe` [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]

  describe "primes" $ do
    it "returns the correct prime numbers" $ do
      takeWhile (< 50) primes `shouldBe` [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47]
