module Shared.Specs.Util.TokenSpec where

import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Shared.Util.Token

tokenSpec =
  describe "Token" $
    describe "separateToken" $ do
      it "'Bearer token1' returns 'token1" $ separateToken "Bearer token1" `shouldBe` Just "token1"
      it "'Bearer ' returns 'Nothing" $ separateToken "Bearer " `shouldBe` Nothing
      it "'token1' returns 'Nothing" $ separateToken "token1" `shouldBe` Nothing
      it "'Bearer sdsa asdasd' returns 'Nothing" $ separateToken "Bearer sdsa asdasd" `shouldBe` Nothing
      it "'' returns 'Nothing" $ separateToken "" `shouldBe` Nothing
