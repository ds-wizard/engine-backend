module Shared.Specs.Common.Util.ListSpec where

import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Shared.Common.Util.List

listSpec =
  describe "List" $ do
    describe "foldMaybe" $ do
      it "[] returns []" $ foldMaybe [] `shouldBe` Just []
      it "[Just a, Just b] returns Just [a, b]" $ foldMaybe [Just 1, Just 2] `shouldBe` Just [1, 2]
      it "[Just a, Nothing, Just b] returns Nothing" $ foldMaybe [Just 1, Nothing, Just 2] `shouldBe` Nothing
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
    describe "dropWhileInclusive" $ do
      it "(<= 3) [] -> []" $ dropWhileInclusive (<= 3) [] `shouldBe` []
      it "(<= 3) [1,2,3,4,5,6] -> [4,5,6]" $ dropWhileInclusive (<= 3) [1, 2, 3, 4, 5, 6] `shouldBe` [4, 5, 6]
      it "(/= 3) [1,2,3,4,5,6] -> [3,4,5,6]" $ dropWhileInclusive (/= 3) [1, 2, 3, 4, 5, 6] `shouldBe` [3, 4, 5, 6]
    describe "dropWhileExclusive" $ do
      it "(<= 3) [] -> []" $ dropWhileExclusive (<= 3) [] `shouldBe` []
      it "(<= 3) [1,2,3,4,5,6] -> [5,6]" $ dropWhileExclusive (<= 3) [1, 2, 3, 4, 5, 6] `shouldBe` [5, 6]
      it "(/= 3) [1,2,3,4,5,6] -> [4,5,6]" $ dropWhileExclusive (/= 3) [1, 2, 3, 4, 5, 6] `shouldBe` [4, 5, 6]
    describe "generateList" $ do
      it "0 -> []" $ generateList 0 `shouldBe` []
      it "1 -> [0]" $ generateList 1 `shouldBe` [0]
      it "2 -> [0,1]" $ generateList 2 `shouldBe` [0, 1]
      it "5 -> [0,1,2,3,4]" $ generateList 5 `shouldBe` [0, 1, 2, 3, 4]
      it "-1 -> []" $ generateList (-1) `shouldBe` []
