module Wizard.Specs.Service.Questionnaire.Migration.Migrator.SanitizerSpec where

import Test.Hspec hiding (shouldBe, shouldNotBe)
import Test.Hspec.Expectations.Pretty

import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Questions
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModelLenses
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireEvents
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireReplies
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Model.Questionnaire.QuestionnaireEventList
import Wizard.Model.Questionnaire.QuestionnaireReply
import Wizard.Service.Questionnaire.Migration.Migrator.Sanitizer

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
          let qtnEvents = fEventsList questionnaire1Uuid
          -- WHEN:
          (Right result) <- runInContext (sanitizeQuestionnaireEvents questionnaire1Uuid oldKm newKm qtnEvents) appContext
          -- THEN:
          extractEventPath (result !! 16) `shouldBe` fst rQ1
          extractEventPath (result !! 17) `shouldBe` fst rQ9
          extractSetEventValue (result !! 17) `shouldBe` (snd rQ9WithNewType).value

extractEventPath :: QuestionnaireEventList -> String
extractEventPath (ClearReplyEventList' event) = event.path
extractEventPath (SetReplyEventList' event) = event.path
extractEventPath _ = error "Expected ClearReplyEventList' or SetReplyEventList'"

extractSetEventValue :: QuestionnaireEventList -> ReplyValue
extractSetEventValue (SetReplyEventList' event) = event.value
extractSetEventValue _ = error "Expected SetReplyEventList'"
