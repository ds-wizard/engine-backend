module Shared.Specs.Common.Util.TokenSpec where

import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Shared.Common.Util.Token

tokenSpec =
  describe "Token" $
    describe "separateToken" $ do
      it "'Bearer token1' returns 'token1" $ separateToken "Bearer token1" `shouldBe` Just "token1"
      it "'Bearer ' returns 'Nothing" $ separateToken "Bearer " `shouldBe` Nothing
      it "'token1' returns 'Nothing" $ separateToken "token1" `shouldBe` Nothing
      it "'Bearer test1 test2' returns 'Nothing" $ separateToken "Bearer test1 test2" `shouldBe` Nothing
      it "'' returns 'Nothing" $ separateToken "" `shouldBe` Nothing
