module Specs.Integration.Http.Common.ResponseMapperSpec where

import Data.Aeson
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Integration.Http.Common.ResponseMapper
import Localization
import Model.Error.Error

import Specs.Integration.Http.Common

commonResponseMapperSpec =
  describe "ResponseMapper" $ do
    describe "extractNestedField" $ do
      it "no nested fields" $
        -- GIVEN: Response
       do
        let targetContent = obj [("targetProp", String "Target Content")]
        let response = targetContent
        -- AND: Expectations
        let expectation = Right targetContent
        -- WHEN:
        let result = extractNestedField [] response
        -- THEN:
        result `shouldBe` expectation
      it "more nested fields" $
        -- GIVEN: Response
       do
        let targetContent = obj [("targetProp", String "Target Content")]
        let response = obj [("firstProp", obj [("secondProp", targetContent)])]
        -- AND: Expectations
        let expectation = Right targetContent
        -- WHEN:
        let result = extractNestedField ["firstProp", "secondProp"] response
        -- THEN:
        result `shouldBe` expectation
      it "non-existing nested fields" $
        -- GIVEN: Response
       do
        let targetContent = obj [("targetProp", String "Target Content")]
        let response = obj [("firstProp", obj [("secondProp", targetContent)])]
        -- AND: Expectations
        let expectation =
              Left . GeneralServerError $
              _ERROR_INTEGRATION_COMMON__RDF_UNABLE_TO_EXTRACT_NESTED_FIELDS ["nonExistingField"]
        -- WHEN:
        let result = extractNestedField ["firstProp", "nonExistingField"] response
        -- THEN:
        result `shouldBe` expectation
    describe "extractStringField" $ do
      it "it works" $
        -- GIVEN: Response
       do
        let response = obj [("targetProp", String "Target Content")]
        -- AND: Expectations
        let expectation = Right "Target Content"
        -- WHEN:
        let result = extractStringField "targetProp" response
        -- THEN:
        result `shouldBe` expectation
      it "bad field type" $
        -- GIVEN: Response
       do
        let response = obj [("targetProp", Number 123)]
        -- AND: Expectations
        let expectation =
              Left . GeneralServerError $ _ERROR_INTEGRATION_COMMON__RDF_UNABLE_TO_EXTRACT_STRING_FIELD "targetProp"
        -- WHEN:
        let result = extractStringField "targetProp" response
        -- THEN:
        result `shouldBe` expectation
      it "non-existing field" $
        -- GIVEN: Response
       do
        let response = obj [("targetProp", String "Target Content")]
        -- AND: Expectations
        let expectation =
              Left . GeneralServerError $
              _ERROR_INTEGRATION_COMMON__RDF_UNABLE_TO_EXTRACT_STRING_FIELD "nonExistingField"
        -- WHEN:
        let result = extractStringField "nonExistingField" response
        -- THEN:
        result `shouldBe` expectation
    describe "convertToArray" $ do
      it "it works" $
        -- GIVEN: Response
       do
        let targetContent = [String "firstValue", String "secondValue"]
        let response = arr targetContent
        -- AND: Expectations
        let expectation = Right targetContent
        -- WHEN:
        let result = convertToArray response
        -- THEN:
        result `shouldBe` expectation
      it "bad type" $
        -- GIVEN: Response
       do
        let response = obj [("targetProp", String "Target Content")]
        -- AND: Expectations
        let expectation = Left . GeneralServerError $ _ERROR_INTEGRATION_COMMON__RDF_FIELD_IS_NOT_ARRAY
        -- WHEN:
        let result = convertToArray response
        -- THEN:
        result `shouldBe` expectation
