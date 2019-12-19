module Shared.Specs.Util.TokenSpec where

import qualified Data.Text as T
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Shared.Util.Token

tokenSpec =
  describe "Token" $ do
    describe "separateToken" $ do
      it "'Bearer token1' returns 'token1" $ separateToken (T.pack "Bearer token1") `shouldBe` Just (T.pack "token1")
      it "'Bearer ' returns 'Nothing" $ separateToken (T.pack "Bearer ") `shouldBe` Nothing
      it "'token1' returns 'Nothing" $ separateToken (T.pack "token1") `shouldBe` Nothing
      it "'Bearer sdsa asdasd' returns 'Nothing" $ separateToken (T.pack "Bearer sdsa asdasd") `shouldBe` Nothing
      it "'' returns 'Nothing" $ separateToken (T.pack "") `shouldBe` Nothing
