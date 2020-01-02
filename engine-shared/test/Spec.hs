module Main where

import Test.Hspec

import Shared.Specs.Model.KnowledgeModel.KnowledgeModelAccessorsSpec
import Shared.Specs.Util.MathSpec

main :: IO ()
main =
  hspec $
  describe "UNIT TESTING" $ do
    describe "MODEL" knowledgeModelAccessorsSpec
    describe "UTIL" mathSpec
