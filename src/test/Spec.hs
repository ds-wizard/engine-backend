module Main where

import Test.Hspec
import Test.Hspec.Expectations.Pretty as TP

import Specs.ApplicatorINTSpec
import Specs.ApplicatorSpec

main :: IO ()
main =
  hspec $ do
    describe "Applicator" $ do
     applicatorSpec
     applicatorINTSpec
