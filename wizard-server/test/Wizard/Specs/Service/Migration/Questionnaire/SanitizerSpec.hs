module Wizard.Specs.Service.Migration.Questionnaire.SanitizerSpec where

import Test.Hspec hiding (shouldBe, shouldNotBe)
import Test.Hspec.Expectations.Pretty

import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireEvents
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireReplies
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Model.Questionnaire.QuestionnaireEvent
import Wizard.Model.Questionnaire.QuestionnaireReply
import Wizard.Service.Migration.Questionnaire.Migrator.Sanitizer
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Questions
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModelLenses

import Wizard.Specs.Common

sanitizerIntegrationSpec appContext =
  describe "Sanitizer" $
    describe "sanitizeQuestionnaireEvents" $
      it "Succeed" $
        -- GIVEN:
        do
          let oldKm = km1WithQ4
          let newKm =
                putInQuestionsM question1.uuid question1WithNewType'
                  . putInQuestionsM question9.uuid question9WithNewType'
                  $ km1WithQ4
          let qtnEvents = fEvents questionnaire1Uuid
          -- WHEN:
          (Right result) <- runInContext (sanitizeQuestionnaireEvents questionnaire1Uuid oldKm newKm qtnEvents) appContext
          -- THEN:
          extractEventPath (result !! 16) `shouldBe` fst rQ1
          extractEventPath (result !! 17) `shouldBe` fst rQ9
          extractSetEventValue (result !! 17) `shouldBe` (snd rQ9WithNewType).value

extractEventPath :: QuestionnaireEvent -> String
extractEventPath (ClearReplyEvent' event) = event.path
extractEventPath (SetReplyEvent' event) = event.path
extractEventPath _ = error "Expected ClearReplyEvent' or SetReplyEvent'"

extractSetEventValue :: QuestionnaireEvent -> ReplyValue
extractSetEventValue (SetReplyEvent' event) = event.value
extractSetEventValue _ = error "Expected SetReplyEvent'"
