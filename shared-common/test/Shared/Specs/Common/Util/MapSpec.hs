module Shared.Specs.Common.Util.MapSpec where

import qualified Data.Map.Strict as M

import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Shared.Common.Util.Map

mapSpec =
  describe "Map" $ do
    describe "doubleGroupBy" $ do
      it "works" $ do
        let input =
              [ ["key1", "key1.1", "1"]
              , ["key1", "key1.2", "2"]
              , ["key2", "key2.1", "3"]
              , ["key2", "key2.2", "4"]
              ]
        let result = doubleGroupBy id id read input
        let expResult =
              M.fromList
                [
                  ( "key1"
                  , M.fromList
                      [ ("key1.1", 1)
                      , ("key1.2", 2)
                      ]
                  )
                ,
                  ( "key2"
                  , M.fromList
                      [ ("key2.1", 3)
                      , ("key2.2", 4)
                      ]
                  )
                ]
        result `shouldBe` expResult
