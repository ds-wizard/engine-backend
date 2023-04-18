module Main where

import Test.Hspec

import WizardLib.Specs.KnowledgeModel.Model.KnowledgeModel.KnowledgeModelAccessorsSpec

main :: IO ()
main =
  hspec $
    describe "UNIT TESTING" $
      describe "KNOWLEDGE MODEL" $
        describe "MODEL" knowledgeModelAccessorsSpec
