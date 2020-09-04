module Wizard.Specs.Service.Questionnaire.Compiler.CompilerServiceSpec where

import Control.Lens ((^.))
import Test.Hspec

import LensesConfig
import Wizard.Api.Resource.Questionnaire.QuestionnaireEventDTO
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireEvents
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Service.Questionnaire.Compiler.CompilerService

questionnaireCompilerServiceSpec =
  describe "Questionnaire Compiler Service" $
  describe "applyEvent" $ do
    it "SetReplyEventDTO" $
        -- GIVEN:
     do
      let event = SetReplyEventDTO' setReplyEvent
        -- WHEN:
      let updatedQtn = applyEvent questionnaire1 event
        -- THEN:
      updatedQtn ^. replies `shouldBe` fRepliesWithUpdated
    it "ClearReplyEventDTO" $
        -- GIVEN:
     do
      let event = ClearReplyEventDTO' clearReplyEvent
        -- WHEN:
      let updatedQtn = applyEvent questionnaire1 event
        -- THEN:
      updatedQtn ^. replies `shouldBe` fRepliesWithDeleted
    it "SetLevelEventDTO" $
        -- GIVEN:
     do
      let event = SetLevelEventDTO' setLevelEvent
        -- WHEN:
      let updatedQtn = applyEvent questionnaire1 event
        -- THEN:
      updatedQtn ^. level `shouldBe` setLevelEvent ^. level
    it "SetLabelsEventDTO" $
        -- GIVEN:
     do
      let event = SetLabelsEventDTO' setLabelsEvent
        -- WHEN:
      let updatedQtn = applyEvent questionnaire1 event
        -- THEN:
      updatedQtn ^. labels `shouldBe` fLabelsEdited
