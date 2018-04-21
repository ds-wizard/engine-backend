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
import Model.Event.Answer.AnswerEvent
import Model.Event.Chapter.ChapterEvent
import Model.Event.Event
import Model.Event.EventField
import Model.Event.FollowUpQuestion.FollowUpQuestionEvent
import Model.Event.KnowledgeModel.KnowledgeModelEvent
import Model.Event.Question.QuestionEvent
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
        resEvent ^. uuid `shouldNotBe` e_km1 ^. uuid
        resEvent ^. chapterIds `shouldBe` ChangedValue [chapter2 ^. uuid, chapter1 ^. uuid]
      it "Event - some KM uuids missing, no new added in event" $
        -- Given:
       do
        let kmChapterIds = _chapterUuid <$> [chapter2]
        let edited_e_km1 = e_km1 & chapterIds .~ ChangedValue kmChapterIds
        let reqState = createTestMigratorStateWithEvents [] [EditKnowledgeModelEvent' edited_e_km1] (Just km1)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditKnowledgeModelEvent' resEvent))) = resState ^. msMigrationState
        resEvent ^. uuid `shouldNotBe` e_km1 ^. uuid
        resEvent ^. chapterIds `shouldBe` ChangedValue [chapter2 ^. uuid, chapter1 ^. uuid]
      it "Event - all KM uuids exists, new added in event but without existing in KM" $
        -- Given:
       do
        let kmChapterIds =
              [chapter2 ^. uuid] ++
              [fromJust . U.fromString $ "54992efb-4738-4f00-9c69-979d28cee5ff"] ++ [chapter1 ^. uuid]
        let edited_e_km1 = e_km1 & chapterIds .~ ChangedValue kmChapterIds
        let reqState = createTestMigratorStateWithEvents [] [EditKnowledgeModelEvent' edited_e_km1] (Just km1)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditKnowledgeModelEvent' resEvent))) = resState ^. msMigrationState
        resEvent ^. uuid `shouldNotBe` e_km1 ^. uuid
        resEvent ^. chapterIds `shouldBe` ChangedValue [chapter2 ^. uuid, chapter1 ^. uuid]
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
        resEvent ^. uuid `shouldNotBe` e_km1_ch1 ^. uuid
        resEvent ^. questionIds `shouldBe` ChangedValue [question2 ^. uuid, question1 ^. uuid]
      it "Event - some KM uuids missing, no new added in event" $
        -- Given:
       do
        let chQuestionIds = _questionUuid <$> [question2]
        let edited_e_km1_ch1 = e_km1_ch1 & questionIds .~ ChangedValue chQuestionIds
        let reqState = createTestMigratorStateWithEvents [] [EditChapterEvent' edited_e_km1_ch1] (Just km1)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditChapterEvent' resEvent))) = resState ^. msMigrationState
        resEvent ^. uuid `shouldNotBe` e_km1_ch1 ^. uuid
        resEvent ^. questionIds `shouldBe` ChangedValue [question2 ^. uuid, question1 ^. uuid]
      it "Event - all KM uuids exists, new added in event but without existing in KM" $
        -- Given:
       do
        let chQuestionIds =
              [question2 ^. uuid] ++
              [fromJust . U.fromString $ "54992efb-4738-4f00-9c69-979d28cee5ff"] ++ [question1 ^. uuid]
        let edited_e_km1_ch1 = e_km1_ch1 & questionIds .~ ChangedValue chQuestionIds
        let reqState = createTestMigratorStateWithEvents [] [EditChapterEvent' edited_e_km1_ch1] (Just km1)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditChapterEvent' resEvent))) = resState ^. msMigrationState
        resEvent ^. uuid `shouldNotBe` e_km1_ch1 ^. uuid
        resEvent ^. questionIds `shouldBe` ChangedValue [question2 ^. uuid, question1 ^. uuid]
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
        resEvent ^. uuid `shouldNotBe` e_km1_ch1_q2 ^. uuid
        resEvent ^. answerIds `shouldBe` (ChangedValue $ Just [answerYes1 ^. uuid, answerNo1 ^. uuid])
        resEvent ^. referenceIds `shouldBe` ChangedValue [referenceCh2 ^. uuid, referenceCh1 ^. uuid]
        resEvent ^. expertIds `shouldBe` ChangedValue [expertLuke ^. uuid, expertDarth ^. uuid]
      it "Event - some KM uuids missing, no new added in event" $
        -- Given:
       do
        let qAnswerIds = Just [answerYes1 ^. uuid]
        let qReferenceIds = [referenceCh2 ^. uuid]
        let qExpertIds = [expertLuke ^. uuid]
        let edited_1_e_km1_ch1_q2 = e_km1_ch1_q2 & answerIds .~ ChangedValue qAnswerIds
        let edited_2_e_km1_ch1_q2 = edited_1_e_km1_ch1_q2 & referenceIds .~ ChangedValue qReferenceIds
        let edited_3_e_km1_ch1_q2 = edited_2_e_km1_ch1_q2 & expertIds .~ ChangedValue qExpertIds
        let reqState = createTestMigratorStateWithEvents [] [EditQuestionEvent' edited_3_e_km1_ch1_q2] (Just km1)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditQuestionEvent' resEvent))) = resState ^. msMigrationState
        resEvent ^. uuid `shouldNotBe` e_km1_ch1_q2 ^. uuid
        resEvent ^. answerIds `shouldBe` (ChangedValue $ Just [answerYes1 ^. uuid, answerNo1 ^. uuid])
        resEvent ^. referenceIds `shouldBe` ChangedValue [referenceCh2 ^. uuid, referenceCh1 ^. uuid]
        resEvent ^. expertIds `shouldBe` ChangedValue [expertLuke ^. uuid, expertDarth ^. uuid]
      it "Event - all KM uuids exists, new added in event but without existing in KM" $
        -- Given:
       do
        let qAnswerIds =
              Just $
              [answerYes1 ^. uuid] ++
              [fromJust . U.fromString $ "54992efb-4738-4f00-9c69-979d28cee5ff"] ++ [answerNo1 ^. uuid]
        let qReferenceIds =
              [referenceCh2 ^. uuid] ++
              [fromJust . U.fromString $ "bdbd95fd-8ea5-485d-9486-ef452b0a661e"] ++ [referenceCh1 ^. uuid]
        let qExpertIds =
              [expertLuke ^. uuid] ++
              [fromJust . U.fromString $ "e47df67f-7e6d-4e0f-950d-5035a48087a0"] ++ [expertDarth ^. uuid]
        let edited_1_e_km1_ch1_q2 = e_km1_ch1_q2 & answerIds .~ ChangedValue qAnswerIds
        let edited_2_e_km1_ch1_q2 = edited_1_e_km1_ch1_q2 & referenceIds .~ ChangedValue qReferenceIds
        let edited_3_e_km1_ch1_q2 = edited_2_e_km1_ch1_q2 & expertIds .~ ChangedValue qExpertIds
        let reqState = createTestMigratorStateWithEvents [] [EditQuestionEvent' edited_3_e_km1_ch1_q2] (Just km1)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditQuestionEvent' resEvent))) = resState ^. msMigrationState
        resEvent ^. uuid `shouldNotBe` e_km1_ch1_q2 ^. uuid
        resEvent ^. answerIds `shouldBe` (ChangedValue $ Just [answerYes1 ^. uuid, answerNo1 ^. uuid])
        resEvent ^. referenceIds `shouldBe` ChangedValue [referenceCh2 ^. uuid, referenceCh1 ^. uuid]
        resEvent ^. expertIds `shouldBe` ChangedValue [expertLuke ^. uuid, expertDarth ^. uuid]
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
        resEvent ^. uuid `shouldNotBe` e_km1_ch1_q2_aYes1_2 ^. uuid
        resEvent ^. followUpIds `shouldBe` ChangedValue [followUpQuestion1 ^. uuid]
      it "Event - some KM uuids missing, no new added in event" $
        -- Given:
       do
        let ansFollowUpIds = []
        let edited_e_km1_ch1_q2_aYes1_2 = e_km1_ch1_q2_aYes1_2 & followUpIds .~ ChangedValue ansFollowUpIds
        let reqState = createTestMigratorStateWithEvents [] [EditAnswerEvent' edited_e_km1_ch1_q2_aYes1_2] (Just km1)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditAnswerEvent' resEvent))) = resState ^. msMigrationState
        resEvent ^. uuid `shouldNotBe` e_km1_ch1_q2_aYes1_2 ^. uuid
        resEvent ^. followUpIds `shouldBe` ChangedValue [followUpQuestion1 ^. uuid]
      it "Event - all KM uuids exists, new added in event but without existing in KM" $
        -- Given:
       do
        let ansFollowUpIds =
              [fromJust . U.fromString $ "54992efb-4738-4f00-9c69-979d28cee5ff"] ++
              [followUpQuestion1 ^. uuid] ++ [fromJust . U.fromString $ "3534d39b-e493-4e9f-b84d-f302c4077b5c"]
        let edited_e_km1_ch1_q2_aYes1_2 = e_km1_ch1_q2_aYes1_2 & followUpIds .~ ChangedValue ansFollowUpIds
        let reqState = createTestMigratorStateWithEvents [] [EditAnswerEvent' edited_e_km1_ch1_q2_aYes1_2] (Just km1)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditAnswerEvent' resEvent))) = resState ^. msMigrationState
        resEvent ^. uuid `shouldNotBe` e_km1_ch1_q2_aYes1_2 ^. uuid
        resEvent ^. followUpIds `shouldBe` ChangedValue [followUpQuestion1 ^. uuid]
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
        resEvent ^. uuid `shouldNotBe` e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2 ^. uuid
        resEvent ^. answerIds `shouldBe` (ChangedValue $ Just [answerYes4 ^. uuid, answerNo4 ^. uuid])
        resEvent ^. referenceIds `shouldBe` ChangedValue []
        resEvent ^. expertIds `shouldBe` ChangedValue []
      it "Event - some KM uuids missing, no new added in event" $
        -- Given:
       do
        let qAnswerIds = Just [answerYes4 ^. uuid]
        let qReferenceIds = [referenceCh2 ^. uuid]
        let qExpertIds = [expertLuke ^. uuid]
        let edited_1_event = e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2 & answerIds .~ ChangedValue qAnswerIds
        let edited_2_event = edited_1_event & referenceIds .~ ChangedValue qReferenceIds
        let edited_3_event = edited_2_event & expertIds .~ ChangedValue qExpertIds
        let reqState = createTestMigratorStateWithEvents [] [EditFollowUpQuestionEvent' edited_3_event] (Just km1)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditFollowUpQuestionEvent' resEvent))) = resState ^. msMigrationState
        resEvent ^. uuid `shouldNotBe` e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2 ^. uuid
        resEvent ^. answerIds `shouldBe` (ChangedValue $ Just [answerYes4 ^. uuid, answerNo4 ^. uuid])
        resEvent ^. referenceIds `shouldBe` ChangedValue []
        resEvent ^. expertIds `shouldBe` ChangedValue []
      it "Event - all KM uuids exists, new added in event but without existing in KM" $
        -- Given:
       do
        let qAnswerIds =
              Just $
              [answerYes4 ^. uuid] ++
              [fromJust . U.fromString $ "54992efb-4738-4f00-9c69-979d28cee5ff"] ++ [answerNo4 ^. uuid]
        let qReferenceIds =
              [referenceCh2 ^. uuid] ++
              [fromJust . U.fromString $ "bdbd95fd-8ea5-485d-9486-ef452b0a661e"] ++ [referenceCh1 ^. uuid]
        let qExpertIds =
              [expertLuke ^. uuid] ++
              [fromJust . U.fromString $ "e47df67f-7e6d-4e0f-950d-5035a48087a0"] ++ [expertDarth ^. uuid]
        let edited_1_event = e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2 & answerIds .~ ChangedValue qAnswerIds
        let edited_2_event = edited_1_event & referenceIds .~ ChangedValue qReferenceIds
        let edited_3_event = edited_2_event & expertIds .~ ChangedValue qExpertIds
        let reqState = createTestMigratorStateWithEvents [] [EditFollowUpQuestionEvent' edited_3_event] (Just km1)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditFollowUpQuestionEvent' resEvent))) = resState ^. msMigrationState
        resEvent ^. uuid `shouldNotBe` e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2 ^. uuid
        resEvent ^. answerIds `shouldBe` (ChangedValue $ Just [answerYes4 ^. uuid, answerNo4 ^. uuid])
        resEvent ^. referenceIds `shouldBe` ChangedValue []
        resEvent ^. expertIds `shouldBe` ChangedValue []
