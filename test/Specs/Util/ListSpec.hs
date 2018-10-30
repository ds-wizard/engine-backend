module Specs.Util.ListSpec where

import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Util.List

listSpec =
  describe "List" $ do
    describe "switchMaybeAndList" $ do
      it "[] returns []" $ switchMaybeAndList [] `shouldBe` Just []
      it "[Just a, Just b] returns Just [a, b]" $ switchMaybeAndList [Just 1, Just 2] `shouldBe` Just [1, 2]
      it "[Just a, Nothing, Just b] returns Nothing" $ switchMaybeAndList [Just 1, Nothing, Just 2] `shouldBe` Nothing
    describe "removeDuplicates" $ do
      it "[] -> []" $ removeDuplicates [] `shouldBe` []
      it "[1,2,3] -> [1,2,3]" $ removeDuplicates [1, 2, 3] `shouldBe` [1, 2, 3]
      it "[1,2,3,1,2] -> [1,2,3,1,2]" $ removeDuplicates [1, 2, 3] `shouldBe` [1, 2, 3]
    describe "elems" $ do
      it "[] -> []" $ [] `elems` [] `shouldBe` True
      it "[] -> [1]" $ [] `elems` [1] `shouldBe` True
      it "[1] -> []" $ [1] `elems` [] `shouldBe` False
      it "[1] -> [1,2]" $ [1] `elems` [1, 2] `shouldBe` True
      it "[1,2] -> [1,2]" $ [1, 2] `elems` [1, 2] `shouldBe` True
      it "[2,1] -> [1,2]" $ [2, 1] `elems` [1, 2] `shouldBe` True
      it "[1,2] -> [1]" $ [1, 2] `elems` [1] `shouldBe` False
