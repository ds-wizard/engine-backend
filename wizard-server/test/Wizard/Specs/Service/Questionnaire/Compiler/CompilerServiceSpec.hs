module Wizard.Specs.Service.Questionnaire.Compiler.CompilerServiceSpec where

import Test.Hspec

import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireEvents
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireLabels
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireReplies
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Model.Questionnaire.QuestionnaireContent
import Wizard.Model.Questionnaire.QuestionnaireEvent
import Wizard.Service.Questionnaire.Compiler.CompilerService

import Wizard.Specs.Common

questionnaireCompilerServiceSpec appContext =
  describe "Questionnaire Compiler Service" $
    describe "applyEvent" $ do
      it "SetReplyEvent" $
        -- GIVEN:
        do
          let event = sre_rQ1Updated'
          -- WHEN:
          (Right (_, updatedQtn)) <- runInContext (applyEvent (return ([], questionnaire1Ctn)) event) appContext
          -- THEN:
          updatedQtn.replies `shouldBe` fRepliesWithUpdated
      it "ClearReplyEvent" $
        -- GIVEN:
        do
          let event = cre_rQ1'
          -- WHEN:
          (Right (_, updatedQtn)) <- runInContext (applyEvent (return ([], questionnaire1Ctn)) event) appContext
          -- THEN:
          updatedQtn.replies `shouldBe` fRepliesWithDeleted
      it "SetPhaseEvent" $
        -- GIVEN:
        do
          let event = sphse_2'
          -- WHEN:
          (Right (_, updatedQtn)) <- runInContext (applyEvent (return ([], questionnaire1Ctn)) event) appContext
          -- THEN:
          updatedQtn.phaseUuid `shouldBe` sphse_2.phaseUuid
      it "SetLabelsEvent" $
        -- GIVEN:
        do
          let event = slble_rQ2'
          -- WHEN:
          (Right (_, updatedQtn)) <- runInContext (applyEvent (return ([], questionnaire1Ctn)) event) appContext
          -- THEN:
          updatedQtn.labels `shouldBe` fLabelsEdited
