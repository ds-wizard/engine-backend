module Wizard.Specs.Integration.Http.Common.ResponseMapperSpec where

import Data.Aeson
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Shared.Model.Error.Error
import Wizard.Integration.Http.Common.ResponseMapper
import Wizard.Localization.Messages.Internal

import Wizard.Specs.Integration.Http.Common

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
      it "it works (for string)" $
        -- GIVEN: Response
       do
        let response = obj [("targetProp", String "Target Content")]
        -- AND: Expectations
        let expectation = Right "Target Content"
        -- WHEN:
        let result = extractStringField "targetProp" response
        -- THEN:
        result `shouldBe` expectation
      it "it works (for int)" $
        -- GIVEN: Response
       do
        let response = obj [("targetProp", Number 123)]
        -- AND: Expectations
        let expectation = Right "123"
        -- WHEN:
        let result = extractStringField "targetProp" response
        -- THEN:
        result `shouldBe` expectation
      it "it works (for double)" $
        -- GIVEN: Response
       do
        let response = obj [("targetProp", Number 123.45)]
        -- AND: Expectations
        let expectation = Right "123.45"
        -- WHEN:
        let result = extractStringField "targetProp" response
        -- THEN:
        result `shouldBe` expectation
      it "it works (for bool)" $
        -- GIVEN: Response
       do
        let response = obj [("targetProp", Bool True)]
        -- AND: Expectations
        let expectation = Right "True"
        -- WHEN:
        let result = extractStringField "targetProp" response
        -- THEN:
        result `shouldBe` expectation
      it "it works (for null)" $
        -- GIVEN: Response
       do
        let response = obj [("targetProp", Null)]
        -- AND: Expectations
        let expectation = Right "null"
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