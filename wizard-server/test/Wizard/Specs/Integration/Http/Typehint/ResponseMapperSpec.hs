module Wizard.Specs.Integration.Http.Typehint.ResponseMapperSpec where

import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Shared.Common.Util.JSON
import Wizard.Integration.Http.Typehint.ResponseMapper
import Wizard.Integration.Resource.Typehint.TypehintIDTO
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Integrations

import Wizard.Specs.Integration.Http.Common

typehintResponseMapperSpec =
  describe "ResponseMapper" $
    describe "toRetrieveTypehintsResponse" $ do
      it "it works (nested fields)" $
        -- GIVEN: Response
        do
          let targetContent =
                obj
                  [
                    ( "nested"
                    , obj
                        [
                          ( "results"
                          , arr
                              [ obj [("id", "op-p000001"), ("name", "Life Science Ontology")]
                              , obj [("id", "op-p000008"), ("name", "Mathematical Ontology")]
                              ]
                          )
                        ]
                    )
                  ]
          let response = createResponse targetContent
          -- AND: Expectations
          let expectation =
                Right
                  [ TypehintIDTO {intId = "op-p000001", name = "Life Science Ontology"}
                  , TypehintIDTO {intId = "op-p000008", name = "Mathematical Ontology"}
                  ]
          -- WHEN:
          let result = toRetrieveTypehintsResponse ontologyPortal response
          -- THEN:
          result `shouldBe` expectation
      it "it works (no nested fields)" $
        -- GIVEN: Response
        do
          let targetContent =
                arr
                  [ obj [("id", "op-p000001"), ("name", "Life Science Ontology")]
                  , obj [("id", "op-p000008"), ("name", "Mathematical Ontology")]
                  ]
          let response = createResponse targetContent
          -- AND: Expectations
          let expectation =
                Right
                  [ TypehintIDTO {intId = "op-p000001", name = "Life Science Ontology"}
                  , TypehintIDTO {intId = "op-p000008", name = "Mathematical Ontology"}
                  ]
          -- WHEN:
          let result = toRetrieveTypehintsResponse bioPortal response
          -- THEN:
          result `shouldBe` expectation
      it "bad 'id' and 'name' mapping" $
        -- GIVEN: Response
        do
          let targetContent =
                arr
                  [ obj [("id", "op-p000001"), ("name", "Life Science Ontology")]
                  , obj [("non-id", "op-p000008"), ("name", "Mathematical Ontology")]
                  ]
          let response = createResponse targetContent
          -- AND: Expectations
          let expectation =
                Right [TypehintIDTO {intId = "op-p000001", name = "Life Science Ontology"}]
          -- WHEN:
          let result = toRetrieveTypehintsResponse bioPortal response
          -- THEN:
          result `shouldBe` expectation
