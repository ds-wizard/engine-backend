module Shared.Specs.Model.Common.PageSpec where

import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata

pageSpec =
  describe "Page" $
    ---------------------------------------------
  describe "mapMaybeP" $ do
    it "empty" $
          -- GIVEN:
     do
      let page = Page "name" (PageMetadata 20 0 0 0) []
      let expPage = Page "name" (PageMetadata 20 0 0 0) []
          -- WHEN:
      let resultPage = mapMaybeP mapPageFn page
          -- THEN:
      expPage `shouldBe` resultPage
    it "full" $
          -- GIVEN:
     do
      let page = Page "name" (PageMetadata 20 4 1 0) ["a", "b", "a", "b"]
      let expPage = Page "name" (PageMetadata 20 2 1 0) ["aa", "aa"]
          -- WHEN:
      let resultPage = mapMaybeP mapPageFn page
          -- THEN:
      expPage `shouldBe` resultPage

mapPageFn "a" = Just "aa"
mapPageFn "b" = Nothing
