module Main where

import Test.Hspec

import WizardLib.Specs.Common.Service.Coordinate.CoordinateValidationSpec
import WizardLib.Specs.KnowledgeModel.Model.KnowledgeModel.KnowledgeModelAccessorsSpec

main :: IO ()
main =
  hspec $
    describe "UNIT TESTING" $ do
      describe "COMMON" $
        describe "SERVICE" $
          describe "Coordinate" coordinateValidationSpec
      describe "KNOWLEDGE MODEL" $
        describe "MODEL" knowledgeModelAccessorsSpec
