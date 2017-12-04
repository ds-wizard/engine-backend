module Specs.Common.UtilsSpec where

import Data.Maybe
import qualified Data.Text as T
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Common.Utils

commonUtilsSpec =
  describe "Common Utils" $ do
    describe "switchMaybeAndList" $ do
      it "[] returns []" $ switchMaybeAndList [] `shouldBe` Just []
      it "[Just a, Just b] returns Just [a, b]" $ switchMaybeAndList [Just 1, Just 2] `shouldBe` Just [1, 2]
      it "[Just a, Nothing, Just b] returns Nothing" $ switchMaybeAndList [Just 1, Nothing, Just 2] `shouldBe` Nothing
    describe "separateToken" $ do
      it "'Bearer token1' returns 'token1" $ separateToken (T.pack "Bearer token1") `shouldBe` Just (T.pack "token1")
      it "'Bearer ' returns 'Nothing" $ separateToken (T.pack "Bearer ") `shouldBe` Nothing
      it "'token1' returns 'Nothing" $ separateToken (T.pack "token1") `shouldBe` Nothing
      it "'Bearer sdsa asdasd' returns 'Nothing" $ separateToken (T.pack "Bearer sdsa asdasd") `shouldBe` Nothing
      it "'' returns 'Nothing" $ separateToken (T.pack "") `shouldBe` Nothing
    describe "removeDuplicates" $ do
      it "[] -> []" $ removeDuplicates [] `shouldBe` []
      it "[1,2,3] -> [1,2,3]" $ removeDuplicates [1,2,3] `shouldBe` [1,2,3]
      it "[1,2,3,1,2] -> [1,2,3,1,2]" $ removeDuplicates [1,2,3] `shouldBe` [1,2,3]
