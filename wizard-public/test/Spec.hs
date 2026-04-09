module Main where

import Test.Hspec

import WizardLib.Public.Specs.Util.JinjaSpec

main :: IO ()
main =
  hspec $
    describe "UNIT TESTING" $
      describe "WIZARD PUBLIC" $
        describe
          "UTIL"
          jinjaSpec
