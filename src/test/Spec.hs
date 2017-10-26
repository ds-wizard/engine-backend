module Main where

import Test.Hspec
import Test.Hspec.Expectations.Pretty

import Specs.ApplicatorINTSpec
import Specs.ApplicatorSpec

main :: IO ()
main =
  hspec $ do
    applicatorSpec
    applicatorINTSpec
