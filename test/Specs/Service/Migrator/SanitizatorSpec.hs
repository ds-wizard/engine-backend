module Specs.Service.Migrator.SanitizatorSpec where

import Control.Lens
import Data.Maybe
import qualified Data.UUID as U
import Test.Hspec hiding (shouldBe, shouldNotBe)
import Test.Hspec.Expectations.Pretty

import Database.Migration.Branch.Data.Event.Event
import Database.Migration.Branch.Data.KnowledgeModel.AnswersAndFollowUpQuestions
import Database.Migration.Branch.Data.KnowledgeModel.Chapters
import Database.Migration.Branch.Data.KnowledgeModel.Experts
import Database.Migration.Branch.Data.KnowledgeModel.KnowledgeModels
import Database.Migration.Branch.Data.KnowledgeModel.Questions
import Database.Migration.Branch.Data.KnowledgeModel.References
import LensesConfig
import Model.Event.Answer.EditAnswerEvent
import Model.Event.Chapter.EditChapterEvent
import Model.Event.Event
import Model.Event.FollowUpQuestion.EditFollowUpQuestionEvent
import Model.Event.KnowledgeModel.EditKnowledgeModelEvent
import Model.Event.Question.AddQuestionEvent
import Model.Event.Question.EditQuestionEvent
import Model.KnowledgeModel.KnowledgeModel
import Model.Migrator.MigratorState
import Service.Migrator.Applicator
import Service.Migrator.Migrator

import Specs.Service.Migrator.Common

sanitizatorSpec =
  describe "Sanatizor" $
    -- -------------------------------------------------------------
    -- -------------------------------------------------------------
   do
    describe "Sanatize EditKnowledgeEvent" $ do
      it "Event - all KM uuids exists, no new added in event" $
        -- Given:
       do
        let reqState = createTestMigratorStateWithEvents [] [EditKnowledgeModelEvent' e_km1] (Just km1)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditKnowledgeModelEvent' resEvent))) = resState ^. msMigrationState
        resEvent ^. ekmUuid `shouldNotBe` e_km1 ^. ekmUuid
        resEvent ^. ekmChapterIds `shouldBe` Just [chapter2 ^. uuid, chapter1 ^. uuid]
      it "Event - some KM uuids missing, no new added in event" $
        -- Given:
       do
        let chapterIds = _chapterUuid <$> [chapter2]
        let edited_e_km1 = e_km1 & ekmChapterIds .~ Just chapterIds
        let reqState = createTestMigratorStateWithEvents [] [EditKnowledgeModelEvent' edited_e_km1] (Just km1)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditKnowledgeModelEvent' resEvent))) = resState ^. msMigrationState
        resEvent ^. ekmUuid `shouldNotBe` e_km1 ^. ekmUuid
        resEvent ^. ekmChapterIds `shouldBe` Just [chapter2 ^. uuid, chapter1 ^. uuid]
      it "Event - all KM uuids exists, new added in event but without existing in KM" $
        -- Given:
       do
        let chapterIds =
              [chapter2 ^. uuid] ++
              [fromJust . U.fromString $ "54992efb-4738-4f00-9c69-979d28cee5ff"] ++ [chapter1 ^. uuid]
        let edited_e_km1 = e_km1 & ekmChapterIds .~ Just chapterIds
        let reqState = createTestMigratorStateWithEvents [] [EditKnowledgeModelEvent' edited_e_km1] (Just km1)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditKnowledgeModelEvent' resEvent))) = resState ^. msMigrationState
        resEvent ^. ekmUuid `shouldNotBe` e_km1 ^. ekmUuid
        resEvent ^. ekmChapterIds `shouldBe` Just [chapter2 ^. uuid, chapter1 ^. uuid]
    -- -------------------------------------------------------------
    -- -------------------------------------------------------------
    describe "Sanatize: EditChapterEvent" $ do
      it "Event - all KM uuids exists, no new added in event" $
        -- Given:
       do
        let reqState = createTestMigratorStateWithEvents [] [EditChapterEvent' e_km1_ch1] (Just km1)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditChapterEvent' resEvent))) = resState ^. msMigrationState
        resEvent ^. echUuid `shouldNotBe` e_km1_ch1 ^. echUuid
        resEvent ^. echQuestionIds `shouldBe` Just [question2 ^. uuid, question1 ^. uuid]
      it "Event - some KM uuids missing, no new added in event" $
        -- Given:
       do
        let questionIds = _questionUuid <$> [question2]
        let edited_e_km1_ch1 = e_km1_ch1 & echQuestionIds .~ Just questionIds
        let reqState = createTestMigratorStateWithEvents [] [EditChapterEvent' edited_e_km1_ch1] (Just km1)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditChapterEvent' resEvent))) = resState ^. msMigrationState
        resEvent ^. echUuid `shouldNotBe` e_km1_ch1 ^. echUuid
        resEvent ^. echQuestionIds `shouldBe` Just [question2 ^. uuid, question1 ^. uuid]
      it "Event - all KM uuids exists, new added in event but without existing in KM" $
        -- Given:
       do
        let questionIds =
              [question2 ^. uuid] ++
              [fromJust . U.fromString $ "54992efb-4738-4f00-9c69-979d28cee5ff"] ++ [question1 ^. uuid]
        let edited_e_km1_ch1 = e_km1_ch1 & echQuestionIds .~ Just questionIds
        let reqState = createTestMigratorStateWithEvents [] [EditChapterEvent' edited_e_km1_ch1] (Just km1)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditChapterEvent' resEvent))) = resState ^. msMigrationState
        resEvent ^. echUuid `shouldNotBe` e_km1_ch1 ^. echUuid
        resEvent ^. echQuestionIds `shouldBe` Just [question2 ^. uuid, question1 ^. uuid]
    -- -------------------------------------------------------------
    -- -------------------------------------------------------------
    describe "Sanatize: EditQuestionEvent" $ do
      it "Event - all KM uuids exists, no new added in event" $
        -- Given:
       do
        let reqState = createTestMigratorStateWithEvents [] [EditQuestionEvent' e_km1_ch1_q2] (Just km1)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditQuestionEvent' resEvent))) = resState ^. msMigrationState
        resEvent ^. eqUuid `shouldNotBe` e_km1_ch1_q2 ^. eqUuid
        resEvent ^. eqAnswerIds `shouldBe` Just [answerYes1 ^. uuid, answerNo1 ^. uuid]
        resEvent ^. eqReferenceIds `shouldBe` Just [referenceCh2 ^. uuid, referenceCh1 ^. uuid]
        resEvent ^. eqExpertIds `shouldBe` Just [expertLuke ^. uuid, expertDarth ^. uuid]
      it "Event - some KM uuids missing, no new added in event" $
        -- Given:
       do
        let answerIds = [answerYes1 ^. uuid]
        let referenceIds = [referenceCh2 ^. uuid]
        let expertIds = [expertLuke ^. uuid]
        let edited_1_e_km1_ch1_q2 = e_km1_ch1_q2 & eqAnswerIds .~ Just answerIds
        let edited_2_e_km1_ch1_q2 = edited_1_e_km1_ch1_q2 & eqReferenceIds .~ Just referenceIds
        let edited_3_e_km1_ch1_q2 = edited_2_e_km1_ch1_q2 & eqExpertIds .~ Just expertIds
        let reqState = createTestMigratorStateWithEvents [] [EditQuestionEvent' edited_3_e_km1_ch1_q2] (Just km1)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditQuestionEvent' resEvent))) = resState ^. msMigrationState
        resEvent ^. eqUuid `shouldNotBe` e_km1_ch1_q2 ^. eqUuid
        resEvent ^. eqAnswerIds `shouldBe` Just [answerYes1 ^. uuid, answerNo1 ^. uuid]
        resEvent ^. eqReferenceIds `shouldBe` Just [referenceCh2 ^. uuid, referenceCh1 ^. uuid]
        resEvent ^. eqExpertIds `shouldBe` Just [expertLuke ^. uuid, expertDarth ^. uuid]
      it "Event - all KM uuids exists, new added in event but without existing in KM" $
        -- Given:
       do
        let answerIds =
              [answerYes1 ^. uuid] ++
              [fromJust . U.fromString $ "54992efb-4738-4f00-9c69-979d28cee5ff"] ++ [answerNo1 ^. uuid]
        let referenceIds =
              [referenceCh2 ^. uuid] ++
              [fromJust . U.fromString $ "bdbd95fd-8ea5-485d-9486-ef452b0a661e"] ++ [referenceCh1 ^. uuid]
        let expertIds =
              [expertLuke ^. uuid] ++
              [fromJust . U.fromString $ "e47df67f-7e6d-4e0f-950d-5035a48087a0"] ++ [expertDarth ^. uuid]
        let edited_1_e_km1_ch1_q2 = e_km1_ch1_q2 & eqAnswerIds .~ Just answerIds
        let edited_2_e_km1_ch1_q2 = edited_1_e_km1_ch1_q2 & eqReferenceIds .~ Just referenceIds
        let edited_3_e_km1_ch1_q2 = edited_2_e_km1_ch1_q2 & eqExpertIds .~ Just expertIds
        let reqState = createTestMigratorStateWithEvents [] [EditQuestionEvent' edited_3_e_km1_ch1_q2] (Just km1)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditQuestionEvent' resEvent))) = resState ^. msMigrationState
        resEvent ^. eqUuid `shouldNotBe` e_km1_ch1_q2 ^. eqUuid
        resEvent ^. eqAnswerIds `shouldBe` Just [answerYes1 ^. uuid, answerNo1 ^. uuid]
        resEvent ^. eqReferenceIds `shouldBe` Just [referenceCh2 ^. uuid, referenceCh1 ^. uuid]
        resEvent ^. eqExpertIds `shouldBe` Just [expertLuke ^. uuid, expertDarth ^. uuid]
    -- -------------------------------------------------------------
    -- -------------------------------------------------------------
    describe "Sanatize: EditAnswerEvent" $ do
      it "Event - all KM uuids exists, no new added in event" $
        -- Given:
       do
        let reqState = createTestMigratorStateWithEvents [] [EditAnswerEvent' e_km1_ch1_q2_aYes1_2] (Just km1)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditAnswerEvent' resEvent))) = resState ^. msMigrationState
        resEvent ^. eansUuid `shouldNotBe` e_km1_ch1_q2_aYes1_2 ^. eansUuid
        resEvent ^. eansFollowUpIds `shouldBe` Just [followUpQuestion1 ^. uuid]
      it "Event - some KM uuids missing, no new added in event" $
        -- Given:
       do
        let followUpIds = []
        let edited_e_km1_ch1_q2_aYes1_2 = e_km1_ch1_q2_aYes1_2 & eansFollowUpIds .~ Just followUpIds
        let reqState = createTestMigratorStateWithEvents [] [EditAnswerEvent' edited_e_km1_ch1_q2_aYes1_2] (Just km1)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditAnswerEvent' resEvent))) = resState ^. msMigrationState
        resEvent ^. eansUuid `shouldNotBe` e_km1_ch1_q2_aYes1_2 ^. eansUuid
        resEvent ^. eansFollowUpIds `shouldBe` Just [followUpQuestion1 ^. uuid]
      it "Event - all KM uuids exists, new added in event but without existing in KM" $
        -- Given:
       do
        let followUpIds =
              [fromJust . U.fromString $ "54992efb-4738-4f00-9c69-979d28cee5ff"] ++
              [followUpQuestion1 ^. uuid] ++ [fromJust . U.fromString $ "3534d39b-e493-4e9f-b84d-f302c4077b5c"]
        let edited_e_km1_ch1_q2_aYes1_2 = e_km1_ch1_q2_aYes1_2 & eansFollowUpIds .~ Just followUpIds
        let reqState = createTestMigratorStateWithEvents [] [EditAnswerEvent' edited_e_km1_ch1_q2_aYes1_2] (Just km1)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditAnswerEvent' resEvent))) = resState ^. msMigrationState
        resEvent ^. eansUuid `shouldNotBe` e_km1_ch1_q2_aYes1_2 ^. eansUuid
        resEvent ^. eansFollowUpIds `shouldBe` Just [followUpQuestion1 ^. uuid]
    -- -------------------------------------------------------------
    -- -------------------------------------------------------------
    describe "Sanatize: EditFollowUpQuestionEvent" $ do
      it "Event - all KM uuids exists, no new added in event" $
        -- Given:
       do
        let reqState =
              createTestMigratorStateWithEvents
                []
                [EditFollowUpQuestionEvent' e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2]
                (Just km1)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditFollowUpQuestionEvent' resEvent))) = resState ^. msMigrationState
        resEvent ^. efuqUuid `shouldNotBe` e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2 ^. efuqUuid
        resEvent ^. efuqAnswerIds `shouldBe` Just [answerYes4 ^. uuid, answerNo4 ^. uuid]
        resEvent ^. efuqReferenceIds `shouldBe` Just []
        resEvent ^. efuqExpertIds `shouldBe` Just []
      it "Event - some KM uuids missing, no new added in event" $
        -- Given:
       do
        let answerIds = [answerYes4 ^. uuid]
        let referenceIds = [referenceCh2 ^. uuid]
        let expertIds = [expertLuke ^. uuid]
        let edited_1_event = e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2 & efuqAnswerIds .~ Just answerIds
        let edited_2_event = edited_1_event & efuqReferenceIds .~ Just referenceIds
        let edited_3_event = edited_2_event & efuqExpertIds .~ Just expertIds
        let reqState = createTestMigratorStateWithEvents [] [EditFollowUpQuestionEvent' edited_3_event] (Just km1)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditFollowUpQuestionEvent' resEvent))) = resState ^. msMigrationState
        resEvent ^. efuqUuid `shouldNotBe` e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2 ^. efuqUuid
        resEvent ^. efuqAnswerIds `shouldBe` Just [answerYes4 ^. uuid, answerNo4 ^. uuid]
        resEvent ^. efuqReferenceIds `shouldBe` Just []
        resEvent ^. efuqExpertIds `shouldBe` Just []
      it "Event - all KM uuids exists, new added in event but without existing in KM" $
        -- Given:
       do
        let answerIds =
              [answerYes4 ^. uuid] ++
              [fromJust . U.fromString $ "54992efb-4738-4f00-9c69-979d28cee5ff"] ++ [answerNo4 ^. uuid]
        let referenceIds =
              [referenceCh2 ^. uuid] ++
              [fromJust . U.fromString $ "bdbd95fd-8ea5-485d-9486-ef452b0a661e"] ++ [referenceCh1 ^. uuid]
        let expertIds =
              [expertLuke ^. uuid] ++
              [fromJust . U.fromString $ "e47df67f-7e6d-4e0f-950d-5035a48087a0"] ++ [expertDarth ^. uuid]
        let edited_1_event = e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2 & efuqAnswerIds .~ Just answerIds
        let edited_2_event = edited_1_event & efuqReferenceIds .~ Just referenceIds
        let edited_3_event = edited_2_event & efuqExpertIds .~ Just expertIds
        let reqState = createTestMigratorStateWithEvents [] [EditFollowUpQuestionEvent' edited_3_event] (Just km1)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditFollowUpQuestionEvent' resEvent))) = resState ^. msMigrationState
        resEvent ^. efuqUuid `shouldNotBe` e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2 ^. efuqUuid
        resEvent ^. efuqAnswerIds `shouldBe` Just [answerYes4 ^. uuid, answerNo4 ^. uuid]
        resEvent ^. efuqReferenceIds `shouldBe` Just []
        resEvent ^. efuqExpertIds `shouldBe` Just []
