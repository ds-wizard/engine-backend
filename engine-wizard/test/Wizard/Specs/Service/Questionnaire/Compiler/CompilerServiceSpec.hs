module Wizard.Specs.Service.Questionnaire.Compiler.CompilerServiceSpec where

import Control.Lens ((^.))
import Test.Hspec

import LensesConfig
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireEvents
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireLabels
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireReplies
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Model.Questionnaire.QuestionnaireEvent
import Wizard.Service.Questionnaire.Compiler.CompilerService

import Wizard.Specs.Common

questionnaireCompilerServiceSpec appContext =
  describe "Questionnaire Compiler Service" $
  describe "applyEvent" $ do
    it "SetReplyEvent" $
        -- GIVEN:
     do
      let event = SetReplyEvent' sre_rQ1Updated
        -- WHEN:
      (Right updatedQtn) <- runInContext (applyEvent (return questionnaire1Ctn) event) appContext
        -- THEN:
      updatedQtn ^. replies `shouldBe` fRepliesWithUpdated
    it "ClearReplyEvent" $
        -- GIVEN:
     do
      let event = ClearReplyEvent' cre_rQ1
        -- WHEN:
      (Right updatedQtn) <- runInContext (applyEvent (return questionnaire1Ctn) event) appContext
        -- THEN:
      updatedQtn ^. replies `shouldBe` fRepliesWithDeleted
    it "SetLevelEvent" $
        -- GIVEN:
     do
      let event = SetLevelEvent' slvle_2
        -- WHEN:
      (Right updatedQtn) <- runInContext (applyEvent (return questionnaire1Ctn) event) appContext
        -- THEN:
      updatedQtn ^. level `shouldBe` slvle_2 ^. level
    it "SetLabelsEvent" $
        -- GIVEN:
     do
      let event = SetLabelsEvent' slble_rQ2
        -- WHEN:
      (Right updatedQtn) <- runInContext (applyEvent (return questionnaire1Ctn) event) appContext
        -- THEN:
      updatedQtn ^. labels `shouldBe` fLabelsEdited
