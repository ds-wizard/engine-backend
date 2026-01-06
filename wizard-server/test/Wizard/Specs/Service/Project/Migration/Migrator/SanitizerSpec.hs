module Wizard.Specs.Service.Project.Migration.Migrator.SanitizerSpec where

import Test.Hspec hiding (shouldBe, shouldNotBe)
import Test.Hspec.Expectations.Pretty

import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Questions
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModelLenses
import Wizard.Database.Migration.Development.Project.Data.ProjectEvents
import Wizard.Database.Migration.Development.Project.Data.ProjectReplies
import Wizard.Database.Migration.Development.Project.Data.Projects
import Wizard.Model.Project.Event.ProjectEventList
import Wizard.Model.Project.ProjectReply
import Wizard.Service.Project.Migration.Migrator.Sanitizer

import Wizard.Specs.Common

sanitizerIntegrationSpec appContext =
  describe "Sanitizer" $
    describe "sanitizeProjectEvents" $
      it "Succeed" $
        -- GIVEN:
        do
          let oldKm = km1WithQ4
          let newKm =
                putInQuestionsM question1.uuid question1WithNewType'
                  . putInQuestionsM question9.uuid question9WithNewType'
                  $ km1WithQ4
          let projectEvents = fEventsList project1Uuid
          -- WHEN:
          (Right result) <- runInContext (sanitizeProjectEvents project1Uuid oldKm newKm projectEvents) appContext
          -- THEN:
          extractEventPath (result !! 16) `shouldBe` fst rQ1
          extractEventPath (result !! 17) `shouldBe` fst rQ9
          extractSetEventValue (result !! 17) `shouldBe` (snd rQ9WithNewType).value

extractEventPath :: ProjectEventList -> String
extractEventPath (ClearReplyEventList' event) = event.path
extractEventPath (SetReplyEventList' event) = event.path
extractEventPath _ = error "Expected ClearReplyEventList' or SetReplyEventList'"

extractSetEventValue :: ProjectEventList -> ReplyValue
extractSetEventValue (SetReplyEventList' event) = event.value
extractSetEventValue _ = error "Expected SetReplyEventList'"
