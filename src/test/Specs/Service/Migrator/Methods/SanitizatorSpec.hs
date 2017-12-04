module Specs.Service.Migrator.Methods.SanitizatorSpec where

import Control.Lens
import Data.Maybe
import qualified Data.UUID as U
import Test.Hspec hiding (shouldBe, shouldNotBe)
import Test.Hspec.Expectations.Pretty

import Model.Event.Event
import Model.KnowledgeModel.KnowledgeModel
import Model.Event.KnowledgeModel.EditKnowledgeModelEvent
import Model.Event.Chapter.EditChapterEvent
import Model.Event.Question.AddQuestionEvent
import Model.Event.Question.EditQuestionEvent
import Model.Event.Answer.EditAnswerEvent
import Model.Event.FollowUpQuestion.EditFollowUpQuestionEvent
import Model.Migrator.MigratorState
import Database.Migration.Branch.Data.KnowledgeModel.KnowledgeModels
import Database.Migration.Branch.Data.KnowledgeModel.Chapters
import Database.Migration.Branch.Data.KnowledgeModel.Questions
import Database.Migration.Branch.Data.KnowledgeModel.AnswersAndFollowUpQuestions
import Database.Migration.Branch.Data.KnowledgeModel.References
import Database.Migration.Branch.Data.KnowledgeModel.Experts
import Database.Migration.Branch.Data.Event.Event
import Service.Migrator.Applicator
import Service.Migrator.Migrator

import Specs.Service.Migrator.Common

sanitizatorSpec =
  describe "Sanatizor" $ do
    -- -------------------------------------------------------------
    -- -------------------------------------------------------------
    describe "Sanatize EditKnowledgeEvent" $ do
      it "Event - all KM uuids exists, no new added in event" $ do
        -- Given:
        let reqState = createTestMigratorStateWithEvents [] [EditKnowledgeModelEvent' e_km1] (Just km1)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditKnowledgeModelEvent' resEvent))) = resState ^. msMigrationState
        resEvent ^. ekmUuid `shouldNotBe` e_km1 ^. ekmUuid
        resEvent ^. ekmChapterIds `shouldBe` Just [chapter2 ^. chUuid, chapter1 ^. chUuid]
      it "Event - some KM uuids missing, no new added in event" $ do
        -- Given:
        let chapterIds = _chUuid <$> [chapter2]
        let edited_e_km1 = e_km1 & ekmChapterIds .~ Just chapterIds
        let reqState = createTestMigratorStateWithEvents [] [EditKnowledgeModelEvent' edited_e_km1] (Just km1)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditKnowledgeModelEvent' resEvent))) = resState ^. msMigrationState
        resEvent ^. ekmUuid `shouldNotBe` e_km1 ^. ekmUuid
        resEvent ^. ekmChapterIds `shouldBe` Just [chapter2 ^. chUuid, chapter1 ^. chUuid]
      it "Event - all KM uuids exists, new added in event but without existing in KM" $ do
        -- Given:
        let chapterIds = [chapter2 ^. chUuid] ++ [fromJust . U.fromString $ "54992efb-4738-4f00-9c69-979d28cee5ff"] ++ [chapter1 ^. chUuid]
        let edited_e_km1 = e_km1 & ekmChapterIds .~ Just chapterIds
        let reqState = createTestMigratorStateWithEvents [] [EditKnowledgeModelEvent' edited_e_km1] (Just km1)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditKnowledgeModelEvent' resEvent))) = resState ^. msMigrationState
        resEvent ^. ekmUuid `shouldNotBe` e_km1 ^. ekmUuid
        resEvent ^. ekmChapterIds `shouldBe` Just [chapter2 ^. chUuid, chapter1 ^. chUuid]
    -- -------------------------------------------------------------
    -- -------------------------------------------------------------
    describe "Sanatize: EditChapterEvent" $ do
      it "Event - all KM uuids exists, no new added in event" $ do
        -- Given:
        let reqState = createTestMigratorStateWithEvents [] [EditChapterEvent' e_km1_ch1] (Just km1)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditChapterEvent' resEvent))) = resState ^. msMigrationState
        resEvent ^. echUuid `shouldNotBe` e_km1_ch1 ^. echUuid
        resEvent ^. echQuestionIds `shouldBe` Just [question2 ^. qUuid, question1 ^. qUuid]
      it "Event - some KM uuids missing, no new added in event" $ do
        -- Given:
        let questionIds = _qUuid <$> [question2]
        let edited_e_km1_ch1 = e_km1_ch1 & echQuestionIds .~ Just questionIds
        let reqState = createTestMigratorStateWithEvents [] [EditChapterEvent' edited_e_km1_ch1] (Just km1)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditChapterEvent' resEvent))) = resState ^. msMigrationState
        resEvent ^. echUuid `shouldNotBe` e_km1_ch1 ^. echUuid
        resEvent ^. echQuestionIds `shouldBe` Just [question2 ^. qUuid, question1 ^. qUuid]
      it "Event - all KM uuids exists, new added in event but without existing in KM" $ do
        -- Given:
        let questionIds = [question2 ^. qUuid] ++ [fromJust . U.fromString $ "54992efb-4738-4f00-9c69-979d28cee5ff"] ++ [question1 ^. qUuid]
        let edited_e_km1_ch1 = e_km1_ch1 & echQuestionIds .~ Just questionIds
        let reqState = createTestMigratorStateWithEvents [] [EditChapterEvent' edited_e_km1_ch1] (Just km1)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditChapterEvent' resEvent))) = resState ^. msMigrationState
        resEvent ^. echUuid `shouldNotBe` e_km1_ch1 ^. echUuid
        resEvent ^. echQuestionIds `shouldBe` Just [question2 ^. qUuid, question1 ^. qUuid]
    -- -------------------------------------------------------------
    -- -------------------------------------------------------------
    describe "Sanatize: EditQuestionEvent" $ do
      it "Event - all KM uuids exists, no new added in event" $ do
        -- Given:
        let reqState = createTestMigratorStateWithEvents [] [EditQuestionEvent' e_km1_ch1_q2] (Just km1)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditQuestionEvent' resEvent))) = resState ^. msMigrationState
        resEvent ^. eqUuid `shouldNotBe` e_km1_ch1_q2 ^. eqUuid
        resEvent ^. eqAnswerIds `shouldBe` Just [answerYes1 ^. ansUuid, answerNo1 ^. ansUuid]
        resEvent ^. eqReferenceIds `shouldBe` Just [referenceCh2 ^. refUuid, referenceCh1 ^. refUuid]
        resEvent ^. eqExpertIds `shouldBe` Just [expertLuke ^. expUuid, expertDarth ^. expUuid]

      it "Event - some KM uuids missing, no new added in event" $ do
        -- Given:
        let answerIds = [answerYes1 ^. ansUuid]
        let referenceIds = [referenceCh2 ^. refUuid]
        let expertIds = [expertLuke ^. expUuid]
        let edited_1_e_km1_ch1_q2 = e_km1_ch1_q2 & eqAnswerIds .~ Just answerIds
        let edited_2_e_km1_ch1_q2 = edited_1_e_km1_ch1_q2 & eqReferenceIds .~ Just referenceIds
        let edited_3_e_km1_ch1_q2 = edited_2_e_km1_ch1_q2 & eqExpertIds .~ Just expertIds
        let reqState = createTestMigratorStateWithEvents [] [EditQuestionEvent' edited_3_e_km1_ch1_q2] (Just km1)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditQuestionEvent' resEvent))) = resState ^. msMigrationState
        resEvent ^. eqUuid `shouldNotBe` e_km1_ch1_q2 ^. eqUuid
        resEvent ^. eqAnswerIds `shouldBe` Just [answerYes1 ^. ansUuid, answerNo1 ^. ansUuid]
        resEvent ^. eqReferenceIds `shouldBe` Just [referenceCh2 ^. refUuid, referenceCh1 ^. refUuid]
        resEvent ^. eqExpertIds `shouldBe` Just [expertLuke ^. expUuid, expertDarth ^. expUuid]
      it "Event - all KM uuids exists, new added in event but without existing in KM" $ do
        -- Given:
        let answerIds = [answerYes1 ^. ansUuid] ++ [fromJust . U.fromString $ "54992efb-4738-4f00-9c69-979d28cee5ff"] ++ [answerNo1 ^. ansUuid]
        let referenceIds = [referenceCh2 ^. refUuid] ++ [fromJust . U.fromString $ "bdbd95fd-8ea5-485d-9486-ef452b0a661e"] ++ [referenceCh1 ^. refUuid]
        let expertIds = [expertLuke ^. expUuid] ++ [fromJust . U.fromString $ "e47df67f-7e6d-4e0f-950d-5035a48087a0"] ++ [expertDarth ^. expUuid]
        let edited_1_e_km1_ch1_q2 = e_km1_ch1_q2 & eqAnswerIds .~ Just answerIds
        let edited_2_e_km1_ch1_q2 = edited_1_e_km1_ch1_q2 & eqReferenceIds .~ Just referenceIds
        let edited_3_e_km1_ch1_q2 = edited_2_e_km1_ch1_q2 & eqExpertIds .~ Just expertIds
        let reqState = createTestMigratorStateWithEvents [] [EditQuestionEvent' edited_3_e_km1_ch1_q2] (Just km1)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditQuestionEvent' resEvent))) = resState ^. msMigrationState
        resEvent ^. eqUuid `shouldNotBe` e_km1_ch1_q2 ^. eqUuid
        resEvent ^. eqAnswerIds `shouldBe` Just [answerYes1 ^. ansUuid, answerNo1 ^. ansUuid]
        resEvent ^. eqReferenceIds `shouldBe` Just [referenceCh2 ^. refUuid, referenceCh1 ^. refUuid]
        resEvent ^. eqExpertIds `shouldBe` Just [expertLuke ^. expUuid, expertDarth ^. expUuid]
    -- -------------------------------------------------------------
    -- -------------------------------------------------------------
    describe "Sanatize: EditAnswerEvent" $ do
      it "Event - all KM uuids exists, no new added in event" $ do
        -- Given:
        let reqState = createTestMigratorStateWithEvents [] [EditAnswerEvent' e_km1_ch1_q2_aYes1_2] (Just km1)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditAnswerEvent' resEvent))) = resState ^. msMigrationState
        resEvent ^. eansUuid `shouldNotBe` e_km1_ch1_q2_aYes1_2 ^. eansUuid
        resEvent ^. eansFollowUpIds `shouldBe` Just [followUpQuestion1 ^. qUuid]
      it "Event - some KM uuids missing, no new added in event" $ do
        -- Given:
        let followUpIds = []
        let edited_e_km1_ch1_q2_aYes1_2 = e_km1_ch1_q2_aYes1_2 & eansFollowUpIds .~ Just followUpIds
        let reqState = createTestMigratorStateWithEvents [] [EditAnswerEvent' edited_e_km1_ch1_q2_aYes1_2] (Just km1)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditAnswerEvent' resEvent))) = resState ^. msMigrationState
        resEvent ^. eansUuid `shouldNotBe` e_km1_ch1_q2_aYes1_2 ^. eansUuid
        resEvent ^. eansFollowUpIds `shouldBe` Just [followUpQuestion1 ^. qUuid]
      it "Event - all KM uuids exists, new added in event but without existing in KM" $ do
        -- Given:
        let followUpIds = [fromJust . U.fromString $ "54992efb-4738-4f00-9c69-979d28cee5ff"] ++ [followUpQuestion1 ^. qUuid] ++ [fromJust . U.fromString $ "3534d39b-e493-4e9f-b84d-f302c4077b5c"]
        let edited_e_km1_ch1_q2_aYes1_2 = e_km1_ch1_q2_aYes1_2 & eansFollowUpIds .~ Just followUpIds
        let reqState = createTestMigratorStateWithEvents [] [EditAnswerEvent' edited_e_km1_ch1_q2_aYes1_2] (Just km1)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditAnswerEvent' resEvent))) = resState ^. msMigrationState
        resEvent ^. eansUuid `shouldNotBe` e_km1_ch1_q2_aYes1_2 ^. eansUuid
        resEvent ^. eansFollowUpIds `shouldBe` Just [followUpQuestion1 ^. qUuid]
    -- -------------------------------------------------------------
    -- -------------------------------------------------------------
    describe "Sanatize: EditFollowUpQuestionEvent" $ do
      it "Event - all KM uuids exists, no new added in event" $ do
        -- Given:
        let reqState = createTestMigratorStateWithEvents [] [EditFollowUpQuestionEvent' e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2] (Just km1)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditFollowUpQuestionEvent' resEvent))) = resState ^. msMigrationState
        resEvent ^. efuqUuid `shouldNotBe` e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2 ^. efuqUuid
        resEvent ^. efuqAnswerIds `shouldBe` Just [answerYes4 ^. ansUuid, answerNo4 ^. ansUuid]
        resEvent ^. efuqReferenceIds `shouldBe` Just []
        resEvent ^. efuqExpertIds `shouldBe` Just []

      it "Event - some KM uuids missing, no new added in event" $ do
        -- Given:
        let answerIds = [answerYes4 ^. ansUuid]
        let referenceIds = [referenceCh2 ^. refUuid]
        let expertIds = [expertLuke ^. expUuid]
        let edited_1_event = e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2 & efuqAnswerIds .~ Just answerIds
        let edited_2_event = edited_1_event & efuqReferenceIds .~ Just referenceIds
        let edited_3_event = edited_2_event & efuqExpertIds .~ Just expertIds
        let reqState = createTestMigratorStateWithEvents [] [EditFollowUpQuestionEvent' edited_3_event] (Just km1)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditFollowUpQuestionEvent' resEvent))) = resState ^. msMigrationState
        resEvent ^. efuqUuid `shouldNotBe` e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2 ^. efuqUuid
        resEvent ^. efuqAnswerIds `shouldBe` Just [answerYes4 ^. ansUuid, answerNo4 ^. ansUuid]
        resEvent ^. efuqReferenceIds `shouldBe` Just []
        resEvent ^. efuqExpertIds `shouldBe` Just []
      it "Event - all KM uuids exists, new added in event but without existing in KM" $ do
        -- Given:
        let answerIds = [answerYes4 ^. ansUuid] ++ [fromJust . U.fromString $ "54992efb-4738-4f00-9c69-979d28cee5ff"] ++ [answerNo4 ^. ansUuid]
        let referenceIds = [referenceCh2 ^. refUuid] ++ [fromJust . U.fromString $ "bdbd95fd-8ea5-485d-9486-ef452b0a661e"] ++ [referenceCh1 ^. refUuid]
        let expertIds = [expertLuke ^. expUuid] ++ [fromJust . U.fromString $ "e47df67f-7e6d-4e0f-950d-5035a48087a0"] ++ [expertDarth ^. expUuid]
        let edited_1_event = e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2 & efuqAnswerIds .~ Just answerIds
        let edited_2_event = edited_1_event & efuqReferenceIds .~ Just referenceIds
        let edited_3_event = edited_2_event & efuqExpertIds .~ Just expertIds
        let reqState = createTestMigratorStateWithEvents [] [EditFollowUpQuestionEvent' edited_3_event] (Just km1)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditFollowUpQuestionEvent' resEvent))) = resState ^. msMigrationState
        resEvent ^. efuqUuid `shouldNotBe` e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2 ^. efuqUuid
        resEvent ^. efuqAnswerIds `shouldBe` Just [answerYes4 ^. ansUuid, answerNo4 ^. ansUuid]
        resEvent ^. efuqReferenceIds `shouldBe` Just []
        resEvent ^. efuqExpertIds `shouldBe` Just []