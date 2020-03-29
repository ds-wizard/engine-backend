module Main where

import Test.Hspec

import Shared.Specs.Model.KnowledgeModel.KnowledgeModelAccessorsSpec
import Shared.Specs.Util.MathSpec
import Shared.Specs.Util.StringSpec
import Shared.Specs.Util.TokenSpec

main :: IO ()
main =
  hspec $
  describe "UNIT TESTING" $ do
    describe "MODEL" knowledgeModelAccessorsSpec
    describe "UTIL" $ do
      mathSpec
      localeSpec
      tokenSpec
