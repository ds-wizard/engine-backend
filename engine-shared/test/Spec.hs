module Main where

import Test.Hspec

import Shared.Specs.Util.MathSpec

main :: IO ()
main = hspec $ do describe "UNIT TESTING" $ do describe "UTIL" $ do mathSpec
