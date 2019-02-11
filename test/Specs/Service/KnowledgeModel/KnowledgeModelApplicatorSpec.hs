module Specs.Service.KnowledgeModel.KnowledgeModelApplicatorSpec where

import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Service.KnowledgeModel.KnowledgeModelApplicator

knowledgeModelApplicatorSpec =
  describe "KnowledgeModelApplicator" $
  describe "filterKnowledgeModel" $ do
    it "No tags provided" $
        -- GIVEN: Prepare inputs
     do
      let tags = []
      let km = km1WithQ4
        -- AND: Prepare expectations
      let expectedKm = km1WithQ4
        -- WHEN:
      let computedKm = filterKnowledgeModel tags km
        -- THEN:
      computedKm `shouldBe` expectedKm
