module Lib.Data.ListSpec (spec) where

import qualified Lib.Data.List as List
import Test.Hspec

spec :: Spec
spec = do
  describe "Lib.Data.List" $ do
    describe "product" $ do
      it "returns identity for empty list" $ do
        List.product [] `shouldBe` (1 :: Int)

      it "returns the product of all items in a list" $ do
        List.product [0 .. 10] `shouldBe` (0 :: Int)
        List.product [1 .. 5] `shouldBe` (120 :: Int)

    describe "product implementations" $ do
      it "are equivalent" $ do
        List.product ([1 .. 10] :: [Int]) `shouldBe` List.product' [1 .. 10]
        List.product ([1 .. 10] :: [Int]) `shouldBe` List.product'' [1 .. 10]
    describe "quickSort" $ do
      it "returns the sorted list" $ do
        List.quickSort ([3, 1, 5, 2, 4] :: [Int]) `shouldBe` [1, 2, 3, 4, 5]
