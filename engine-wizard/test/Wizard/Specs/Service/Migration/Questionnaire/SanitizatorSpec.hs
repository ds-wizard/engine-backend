module Wizard.Specs.Service.Migration.Questionnaire.SanitizatorSpec where

import Control.Lens ((?~), (^.), at)
import Test.Hspec hiding (shouldBe, shouldNotBe)
import Test.Hspec.Expectations.Pretty

import LensesConfig
import Shared.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Shared.Database.Migration.Development.KnowledgeModel.Data.Questions
import Shared.Model.KnowledgeModel.KnowledgeModelLenses
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireEvents
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireReplies
import Wizard.Model.Questionnaire.QuestionnaireEvent
import Wizard.Model.Questionnaire.QuestionnaireReply
import Wizard.Service.Migration.Questionnaire.Migrator.Sanitizator

import Wizard.Specs.Common

sanitizatorIntegrationSpec appContext =
  describe "Sanatizator" $ describe "sanitizeQuestionnaireEvents" $ it "Succeed" $
         -- GIVEN:
   do
    let oldKm = km1WithQ4
    let newKm =
          (questionsM . at (question1 ^. uuid) ?~ question1WithNewType') .
          (questionsM . at (question9 ^. uuid) ?~ question9WithNewType') $
          km1WithQ4
    let qtnEvents = fEvents
         -- WHEN:
    (Right result) <- runInContext (sanitizeQuestionnaireEvents oldKm newKm qtnEvents) appContext
         -- THEN:
    extractClearEventPath (result !! 16) `shouldBe` fst rQ1
    extractSetEventPath (result !! 17) `shouldBe` fst rQ9
    extractSetEventValue (result !! 17) `shouldBe` snd rQ9WithNewType ^. value

extractClearEventPath :: QuestionnaireEvent -> String
extractClearEventPath (ClearReplyEvent' event) = event ^. path

extractSetEventPath :: QuestionnaireEvent -> String
extractSetEventPath (SetReplyEvent' event) = event ^. path

extractSetEventValue :: QuestionnaireEvent -> ReplyValue
extractSetEventValue (SetReplyEvent' event) = event ^. value
