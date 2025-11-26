module Wizard.Specs.Service.Questionnaire.Compiler.CompilerServiceSpec where

import Test.Hspec

import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireEvents
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireLabels
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireReplies
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Questionnaire.QuestionnaireContent
import Wizard.Model.Questionnaire.QuestionnaireEvent
import Wizard.Service.Questionnaire.Compiler.CompilerService
import Wizard.Service.Questionnaire.Event.QuestionnaireEventMapper

questionnaireCompilerServiceSpec =
  describe "Questionnaire Compiler Service" $
    describe "applyEvent" $ do
      it "SetReplyEvent" $
        -- GIVEN:
        do
          let event = toEventList (sre_rQ1Updated' questionnaire1Uuid) (Just userAlbert)
          -- WHEN:
          let updatedQtnCtn = applyEvent questionnaire1Ctn event
          -- THEN:
          updatedQtnCtn.replies `shouldBe` fRepliesWithUpdated
      it "ClearReplyEvent" $
        -- GIVEN:
        do
          let event = toEventList (cre_rQ1' questionnaire1Uuid) (Just userAlbert)
          -- WHEN:
          let updatedQtnCtn = applyEvent questionnaire1Ctn event
          -- THEN:
          updatedQtnCtn.replies `shouldBe` fRepliesWithDeleted
      it "SetPhaseEvent" $
        -- GIVEN:
        do
          let event = toEventList (sphse_2' questionnaire1Uuid) (Just userAlbert)
          -- WHEN:
          let updatedQtnCtn = applyEvent questionnaire1Ctn event
          -- THEN:
          updatedQtnCtn.phaseUuid `shouldBe` (sphse_2 questionnaire1Uuid).phaseUuid
      it "SetLabelsEvent" $
        -- GIVEN:
        do
          let event = toEventList (slble_rQ2' questionnaire1Uuid) (Just userAlbert)
          -- WHEN:
          let updatedQtnCtn = applyEvent questionnaire1Ctn event
          -- THEN:
          updatedQtnCtn.labels `shouldBe` fLabelsEdited
