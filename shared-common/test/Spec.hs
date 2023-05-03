module Main where

import Test.Hspec

import Shared.Specs.Common.Integration.Http.Common.ResponseMapperSpec
import Shared.Specs.Common.Model.Common.PageSpec
import Shared.Specs.Common.Util.ListSpec
import Shared.Specs.Common.Util.MathSpec
import Shared.Specs.Common.Util.StringSpec (localeSpec)
import Shared.Specs.Common.Util.TokenSpec

main :: IO ()
main =
  hspec $
    describe "UNIT TESTING" $
      describe "COMMON" $ do
        describe "INTEGRATION" $
          describe "Http" $ do
            describe "Common" commonResponseMapperSpec
        describe "MODEL" $ do
          pageSpec
        describe "UTIL" $ do
          mathSpec
          listSpec
          localeSpec
          tokenSpec
