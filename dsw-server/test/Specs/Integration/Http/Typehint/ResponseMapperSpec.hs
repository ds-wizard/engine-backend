module Specs.Integration.Http.Typehint.ResponseMapperSpec where

import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Database.Migration.Development.KnowledgeModel.Data.Integrations
import Integration.Http.Typehint.ResponseMapper
import Integration.Resource.Typehint.TypehintIDTO
import Localization
import Model.Error.Error

import Specs.Integration.Http.Common

typehintResponseMapperSpec =
  describe "ResponseMapper" $ do
    describe "toRetrieveTypehintsResponse" $ do
      it "it works (nested fields)" $
        -- GIVEN: Response
       do
        let targetContent =
              obj
                [ ( "nested"
                  , obj
                      [ ( "results"
                        , arr
                            [ obj [("id", "op-p000001"), ("name", "Life Science Ontology")]
                            , obj [("id", "op-p000008"), ("name", "Mathematical Ontology")]
                            ])
                      ])
                ]
        let response = createResponse targetContent
        -- AND: Expectations
        let expectation =
              Right
                [ TypehintIDTO {_typehintIDTOIntId = "op-p000001", _typehintIDTOName = "Life Science Ontology"}
                , TypehintIDTO {_typehintIDTOIntId = "op-p000008", _typehintIDTOName = "Mathematical Ontology"}
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
                [ TypehintIDTO {_typehintIDTOIntId = "op-p000001", _typehintIDTOName = "Life Science Ontology"}
                , TypehintIDTO {_typehintIDTOIntId = "op-p000008", _typehintIDTOName = "Mathematical Ontology"}
                ]
        -- WHEN:
        let result = toRetrieveTypehintsResponse bioPortal response
        -- THEN:
        result `shouldBe` expectation
      it "bad 'id' and 'name' mapping" $
        -- GIVEN: Response
       do
        do let targetContent =
                 arr
                   [ obj [("id", "op-p000001"), ("name", "Life Science Ontology")]
                   , obj [("non-id", "op-p000008"), ("name", "Mathematical Ontology")]
                   ]
           let response = createResponse targetContent
        -- AND: Expectations
           let expectation =
                 Left . GeneralServerError $ _ERROR_INTEGRATION_COMMON__RDF_UNABLE_TO_EXTRACT_STRING_FIELD "id"
        -- WHEN:
           let result = toRetrieveTypehintsResponse bioPortal response
        -- THEN:
           result `shouldBe` expectation
