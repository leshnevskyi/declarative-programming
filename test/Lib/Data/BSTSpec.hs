module Lib.Data.BSTSpec (spec) where

import Lib.Data.BST
import Test.Hspec

spec :: Spec
spec = do
  describe "singleton" $ do
    it "creates a BST with a single element" $ do
      singleton 5 `shouldBe` Node 5 Nil Nil

  describe "insert" $ do
    it "inserts an element into an empty BST" $ do
      insert 5 Nil `shouldBe` Node 5 Nil Nil

    it "inserts an element into a non-empty BST" $ do
      let bst = singleton 5
          bst' = insert 3 bst
      bst' `shouldBe` Node 5 (Node 3 Nil Nil) Nil

  describe "fromList" $ do
    it "creates a BST from a list of elements" $ do
      fromList [5, 3, 7] `shouldBe` Node 5 (Node 3 Nil Nil) (Node 7 Nil Nil)

  describe "findByLevel" $ do
    it "returns a list of elements at a specific level in the BST" $ do
      findByLevel 1 (Node 5 (Node 3 (Node 2 Nil Nil) Nil) (Node 7 Nil Nil)) `shouldBe` [3, 7]
