module Main where

import Test.Hspec

import Shared.Specs.Model.Common.PageSpec
import Shared.Specs.Model.KnowledgeModel.KnowledgeModelAccessorsSpec
import Shared.Specs.Util.ListSpec
import Shared.Specs.Util.MathSpec
import Shared.Specs.Util.StringSpec
import Shared.Specs.Util.TokenSpec

main :: IO ()
main =
  hspec $
  describe "UNIT TESTING" $ do
    describe "MODEL" $ do
      knowledgeModelAccessorsSpec
      pageSpec
    describe "UTIL" $ do
      mathSpec
      listSpec
      localeSpec
      tokenSpec
