module Specs.Service.Migrator.SanitizatorSpec where

import Control.Lens
import Data.Maybe
import qualified Data.UUID as U
import Test.Hspec hiding (shouldBe, shouldNotBe)
import Test.Hspec.Expectations.Pretty

import Database.Migration.Development.Event.Data.Events
import Database.Migration.Development.KnowledgeModel.Data.AnswersAndFollowUpQuestions
import Database.Migration.Development.KnowledgeModel.Data.Chapters
import Database.Migration.Development.KnowledgeModel.Data.Experts
import Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Database.Migration.Development.KnowledgeModel.Data.Questions
import Database.Migration.Development.KnowledgeModel.Data.References
import LensesConfig
import Model.Event.Event
import Model.Event.EventField
import Model.KnowledgeModel.KnowledgeModel
import Model.Migrator.MigratorState
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
        let (ConflictState (CorrectorConflict (EditKnowledgeModelEvent' resEvent))) = resState ^. migrationState
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
        let (ConflictState (CorrectorConflict (EditKnowledgeModelEvent' resEvent))) = resState ^. migrationState
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
        let (ConflictState (CorrectorConflict (EditKnowledgeModelEvent' resEvent))) = resState ^. migrationState
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
        let (ConflictState (CorrectorConflict (EditChapterEvent' resEvent))) = resState ^. migrationState
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
        let (ConflictState (CorrectorConflict (EditChapterEvent' resEvent))) = resState ^. migrationState
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
        let (ConflictState (CorrectorConflict (EditChapterEvent' resEvent))) = resState ^. migrationState
        resEvent ^. uuid `shouldNotBe` e_km1_ch1 ^. uuid
        resEvent ^. questionIds `shouldBe` ChangedValue [question2 ^. uuid, question1 ^. uuid]
    -- -------------------------------------------------------------
    -- -------------------------------------------------------------
    describe "Sanatize: EditQuestionEvent" $ do
      it "QType - QuestionTypeOptions: Event - all KM uuids exists, no new added in event" $
        -- Given:
       do
        let reqState = createTestMigratorStateWithEvents [] [EditQuestionEvent' e_km1_ch1_q2] (Just km1)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditQuestionEvent' resEvent))) = resState ^. migrationState
        resEvent ^. uuid `shouldNotBe` e_km1_ch1_q2 ^. uuid
        resEvent ^. answerIds `shouldBe` (ChangedValue $ Just [q2_answerYes ^. uuid, q2_answerNo ^. uuid])
        resEvent ^. referenceIds `shouldBe` ChangedValue [referenceCh2 ^. uuid, referenceCh1 ^. uuid]
        resEvent ^. expertIds `shouldBe` ChangedValue [expertNikola ^. uuid, expertAlbert ^. uuid]
      it "QType - QuestionTypeOptions: Event - some KM uuids missing, no new added in event" $
        -- Given:
       do
        let qAnswerIds = Just [q2_answerYes ^. uuid]
        let qReferenceIds = [referenceCh2 ^. uuid]
        let qExpertIds = [expertNikola ^. uuid]
        let edited_1_e_km1_ch1_q2 = e_km1_ch1_q2 & answerIds .~ ChangedValue qAnswerIds
        let edited_2_e_km1_ch1_q2 = edited_1_e_km1_ch1_q2 & referenceIds .~ ChangedValue qReferenceIds
        let edited_3_e_km1_ch1_q2 = edited_2_e_km1_ch1_q2 & expertIds .~ ChangedValue qExpertIds
        let reqState = createTestMigratorStateWithEvents [] [EditQuestionEvent' edited_3_e_km1_ch1_q2] (Just km1)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditQuestionEvent' resEvent))) = resState ^. migrationState
        resEvent ^. uuid `shouldNotBe` e_km1_ch1_q2 ^. uuid
        resEvent ^. answerIds `shouldBe` (ChangedValue $ Just [q2_answerYes ^. uuid, q2_answerNo ^. uuid])
        resEvent ^. referenceIds `shouldBe` ChangedValue [referenceCh2 ^. uuid, referenceCh1 ^. uuid]
        resEvent ^. expertIds `shouldBe` ChangedValue [expertNikola ^. uuid, expertAlbert ^. uuid]
      it "QType - QuestionTypeOptions: Event - all KM uuids exists, new added in event but without existing in KM" $
          -- Given:
       do
        let qAnswerIds =
              Just $
              [q2_answerYes ^. uuid] ++
              [fromJust . U.fromString $ "54992efb-4738-4f00-9c69-979d28cee5ff"] ++ [q2_answerNo ^. uuid]
        let qReferenceIds =
              [referenceCh2 ^. uuid] ++
              [fromJust . U.fromString $ "bdbd95fd-8ea5-485d-9486-ef452b0a661e"] ++ [referenceCh1 ^. uuid]
        let qExpertIds =
              [expertNikola ^. uuid] ++
              [fromJust . U.fromString $ "e47df67f-7e6d-4e0f-950d-5035a48087a0"] ++ [expertAlbert ^. uuid]
        let edited_1_e_km1_ch1_q2 = e_km1_ch1_q2 & answerIds .~ ChangedValue qAnswerIds
        let edited_2_e_km1_ch1_q2 = edited_1_e_km1_ch1_q2 & referenceIds .~ ChangedValue qReferenceIds
        let edited_3_e_km1_ch1_q2 = edited_2_e_km1_ch1_q2 & expertIds .~ ChangedValue qExpertIds
        let reqState = createTestMigratorStateWithEvents [] [EditQuestionEvent' edited_3_e_km1_ch1_q2] (Just km1)
          -- When:
        resState <- migrate reqState
          -- Then:
        let (ConflictState (CorrectorConflict (EditQuestionEvent' resEvent))) = resState ^. migrationState
        resEvent ^. uuid `shouldNotBe` e_km1_ch1_q2 ^. uuid
        resEvent ^. answerIds `shouldBe` (ChangedValue $ Just [q2_answerYes ^. uuid, q2_answerNo ^. uuid])
        resEvent ^. referenceIds `shouldBe` ChangedValue [referenceCh2 ^. uuid, referenceCh1 ^. uuid]
        resEvent ^. expertIds `shouldBe` ChangedValue [expertNikola ^. uuid, expertAlbert ^. uuid]
      -- -------------------------------------------------------------
      it "QType - QuestionTypeList: Event - all KM uuids exists, no new added in event" $
        -- Given:
       do
        let reqState = createTestMigratorStateWithEvents [] [EditQuestionEvent' e_km1_ch2_q4] (Just km1WithQ4)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditQuestionEvent' resEvent))) = resState ^. migrationState
        resEvent ^. uuid `shouldNotBe` e_km1_ch1_q2 ^. uuid
        let (ChangedValue (Just resAit)) = resEvent ^. answerItemTemplatePlainWithIds
        resAit ^. questionIds `shouldBe` [q4_ait1_question6 ^. uuid, q4_ait1_question5 ^. uuid]
      it "QType - QuestionTypeList: Event - some KM uuids missing, no new added in event" $
        -- Given:
       do
        let aitQuestionIds = [q4_ait1_question6 ^. uuid]
        let aitPlainWithIds =
              AnswerItemTemplatePlainWithIds
              { _answerItemTemplatePlainWithIdsTitle = q4_ait ^. title
              , _answerItemTemplatePlainWithIdsQuestionIds = aitQuestionIds
              }
        let edited_1_e_km1_ch2_q4 = e_km1_ch2_q4 & answerItemTemplatePlainWithIds .~ ChangedValue (Just aitPlainWithIds)
        let reqState = createTestMigratorStateWithEvents [] [EditQuestionEvent' edited_1_e_km1_ch2_q4] (Just km1WithQ4)
           -- When:
        resState <- migrate reqState
           -- Then:
        let (ConflictState (CorrectorConflict (EditQuestionEvent' resEvent))) = resState ^. migrationState
        resEvent ^. uuid `shouldNotBe` e_km1_ch2_q4 ^. uuid
        let (ChangedValue (Just resAit)) = resEvent ^. answerItemTemplatePlainWithIds
        resAit ^. questionIds `shouldBe` [q4_ait1_question6 ^. uuid, q4_ait1_question5 ^. uuid]
      it "QType - QuestionTypeList: Event - all KM uuids exists, new added in event but without existing in KM" $
          -- Given:
       do
        let aitQuestionIds =
              [q4_ait1_question6 ^. uuid] ++
              [fromJust . U.fromString $ "54992efb-4738-4f00-9c69-979d28cee5ff"] ++ [q4_ait1_question5 ^. uuid]
        let aitPlainWithIds =
              AnswerItemTemplatePlainWithIds
              { _answerItemTemplatePlainWithIdsTitle = q4_ait ^. title
              , _answerItemTemplatePlainWithIdsQuestionIds = aitQuestionIds
              }
        let edited_1_e_km1_ch2_q4 = e_km1_ch2_q4 & answerItemTemplatePlainWithIds .~ ChangedValue (Just aitPlainWithIds)
        let reqState = createTestMigratorStateWithEvents [] [EditQuestionEvent' edited_1_e_km1_ch2_q4] (Just km1WithQ4)
          -- When:
        resState <- migrate reqState
          -- Then:
        let (ConflictState (CorrectorConflict (EditQuestionEvent' resEvent))) = resState ^. migrationState
        resEvent ^. uuid `shouldNotBe` e_km1_ch2_q4 ^. uuid
        let (ChangedValue (Just resAit)) = resEvent ^. answerItemTemplatePlainWithIds
        resAit ^. questionIds `shouldBe` [q4_ait1_question6 ^. uuid, q4_ait1_question5 ^. uuid]
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
        let (ConflictState (CorrectorConflict (EditAnswerEvent' resEvent))) = resState ^. migrationState
        resEvent ^. uuid `shouldNotBe` e_km1_ch1_q2_aYes1_2 ^. uuid
        resEvent ^. followUpIds `shouldBe` ChangedValue [q2_aYes_fuQuestion1 ^. uuid]
      it "Event - some KM uuids missing, no new added in event" $
        -- Given:
       do
        let ansFollowUpIds = []
        let edited_e_km1_ch1_q2_aYes1_2 = e_km1_ch1_q2_aYes1_2 & followUpIds .~ ChangedValue ansFollowUpIds
        let reqState = createTestMigratorStateWithEvents [] [EditAnswerEvent' edited_e_km1_ch1_q2_aYes1_2] (Just km1)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditAnswerEvent' resEvent))) = resState ^. migrationState
        resEvent ^. uuid `shouldNotBe` e_km1_ch1_q2_aYes1_2 ^. uuid
        resEvent ^. followUpIds `shouldBe` ChangedValue [q2_aYes_fuQuestion1 ^. uuid]
      it "Event - all KM uuids exists, new added in event but without existing in KM" $
        -- Given:
       do
        let ansFollowUpIds =
              [fromJust . U.fromString $ "54992efb-4738-4f00-9c69-979d28cee5ff"] ++
              [q2_aYes_fuQuestion1 ^. uuid] ++ [fromJust . U.fromString $ "3534d39b-e493-4e9f-b84d-f302c4077b5c"]
        let edited_e_km1_ch1_q2_aYes1_2 = e_km1_ch1_q2_aYes1_2 & followUpIds .~ ChangedValue ansFollowUpIds
        let reqState = createTestMigratorStateWithEvents [] [EditAnswerEvent' edited_e_km1_ch1_q2_aYes1_2] (Just km1)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditAnswerEvent' resEvent))) = resState ^. migrationState
        resEvent ^. uuid `shouldNotBe` e_km1_ch1_q2_aYes1_2 ^. uuid
        resEvent ^. followUpIds `shouldBe` ChangedValue [q2_aYes_fuQuestion1 ^. uuid]
    -- -------------------------------------------------------------
    -- -------------------------------------------------------------
    describe "Sanatize: EditQuestionEvent (FollowUps)" $ do
      it "QType - QuestionTypeOptions: Event - all KM uuids exists, no new added in event" $
        -- Given:
       do
        let reqState =
              createTestMigratorStateWithEvents [] [EditQuestionEvent' e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2] (Just km1)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditQuestionEvent' resEvent))) = resState ^. migrationState
        resEvent ^. uuid `shouldNotBe` e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2 ^. uuid
        resEvent ^. answerIds `shouldBe`
          (ChangedValue $ Just [q2_aYes_fuq1_aYes_fuq2_answerYes ^. uuid, q2_aYes_fuq1_aYes_fuq2_answerNo ^. uuid])
        resEvent ^. referenceIds `shouldBe` ChangedValue []
        resEvent ^. expertIds `shouldBe` ChangedValue []
      it "QType - QuestionTypeOptions: Event - some KM uuids missing, no new added in event" $
        -- Given:
       do
        let qAnswerIds = Just [q2_aYes_fuq1_aYes_fuq2_answerYes ^. uuid]
        let qReferenceIds = [referenceCh2 ^. uuid]
        let qExpertIds = [expertNikola ^. uuid]
        let edited_1_event = e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2 & answerIds .~ ChangedValue qAnswerIds
        let edited_2_event = edited_1_event & referenceIds .~ ChangedValue qReferenceIds
        let edited_3_event = edited_2_event & expertIds .~ ChangedValue qExpertIds
        let reqState = createTestMigratorStateWithEvents [] [EditQuestionEvent' edited_3_event] (Just km1)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditQuestionEvent' resEvent))) = resState ^. migrationState
        resEvent ^. uuid `shouldNotBe` e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2 ^. uuid
        resEvent ^. answerIds `shouldBe`
          (ChangedValue $ Just [q2_aYes_fuq1_aYes_fuq2_answerYes ^. uuid, q2_aYes_fuq1_aYes_fuq2_answerNo ^. uuid])
        resEvent ^. referenceIds `shouldBe` ChangedValue []
        resEvent ^. expertIds `shouldBe` ChangedValue []
      it "QType - QuestionTypeOptions: Event - all KM uuids exists, new added in event but without existing in KM" $
        -- Given:
       do
        let qAnswerIds =
              Just $
              [q2_aYes_fuq1_aYes_fuq2_answerYes ^. uuid] ++
              [fromJust . U.fromString $ "54992efb-4738-4f00-9c69-979d28cee5ff"] ++
              [q2_aYes_fuq1_aYes_fuq2_answerNo ^. uuid]
        let qReferenceIds =
              [referenceCh2 ^. uuid] ++
              [fromJust . U.fromString $ "bdbd95fd-8ea5-485d-9486-ef452b0a661e"] ++ [referenceCh1 ^. uuid]
        let qExpertIds =
              [expertNikola ^. uuid] ++
              [fromJust . U.fromString $ "e47df67f-7e6d-4e0f-950d-5035a48087a0"] ++ [expertAlbert ^. uuid]
        let edited_1_event = e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2 & answerIds .~ ChangedValue qAnswerIds
        let edited_2_event = edited_1_event & referenceIds .~ ChangedValue qReferenceIds
        let edited_3_event = edited_2_event & expertIds .~ ChangedValue qExpertIds
        let reqState = createTestMigratorStateWithEvents [] [EditQuestionEvent' edited_3_event] (Just km1)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditQuestionEvent' resEvent))) = resState ^. migrationState
        resEvent ^. uuid `shouldNotBe` e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2 ^. uuid
        resEvent ^. answerIds `shouldBe`
          (ChangedValue $ Just [q2_aYes_fuq1_aYes_fuq2_answerYes ^. uuid, q2_aYes_fuq1_aYes_fuq2_answerNo ^. uuid])
        resEvent ^. referenceIds `shouldBe` ChangedValue []
        resEvent ^. expertIds `shouldBe` ChangedValue []
      -- -------------------------------------------------------------
      it "QType - QuestionTypeList: Event - all KM uuids exists, no new added in event" $
        -- Given:
       do
        let reqState =
              createTestMigratorStateWithEvents [] [EditQuestionEvent' e_km1_ch2_ansMaybe6_fuq4] (Just km1WithQ4)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditQuestionEvent' resEvent))) = resState ^. migrationState
        resEvent ^. uuid `shouldNotBe` e_km1_ch2_ansMaybe6_fuq4 ^. uuid
        let (ChangedValue (Just resAit)) = resEvent ^. answerItemTemplatePlainWithIds
        resAit ^. questionIds `shouldBe`
          [q4_ait1_q6_aYes_fuq4_ait_question2 ^. uuid, q4_ait1_q6_aYes_fuq4_ait_question1 ^. uuid]
      it "QType - QuestionTypeList: Event - some KM uuids missing, no new added in event" $
        -- Given:
       do
        let aitQuestionIds = [q4_ait1_q6_aYes_fuq4_ait_question2 ^. uuid]
        let aitPlainWithIds =
              AnswerItemTemplatePlainWithIds
              { _answerItemTemplatePlainWithIdsTitle = q4_ait ^. title
              , _answerItemTemplatePlainWithIdsQuestionIds = aitQuestionIds
              }
        let edited_e_km1_ch2_ansMaybe6_fuq4 =
              e_km1_ch2_ansMaybe6_fuq4 & answerItemTemplatePlainWithIds .~ ChangedValue (Just aitPlainWithIds)
        let reqState =
              createTestMigratorStateWithEvents [] [EditQuestionEvent' edited_e_km1_ch2_ansMaybe6_fuq4] (Just km1WithQ4)
           -- When:
        resState <- migrate reqState
           -- Then:
        let (ConflictState (CorrectorConflict (EditQuestionEvent' resEvent))) = resState ^. migrationState
        resEvent ^. uuid `shouldNotBe` e_km1_ch2_ansMaybe6_fuq4 ^. uuid
        let (ChangedValue (Just resAit)) = resEvent ^. answerItemTemplatePlainWithIds
        resAit ^. questionIds `shouldBe`
          [q4_ait1_q6_aYes_fuq4_ait_question2 ^. uuid, q4_ait1_q6_aYes_fuq4_ait_question1 ^. uuid]
      it "QType - QuestionTypeList: Event - all KM uuids exists, new added in event but without existing in KM" $
          -- Given:
       do
        let aitQuestionIds =
              [q4_ait1_q6_aYes_fuq4_ait_question2 ^. uuid] ++
              [fromJust . U.fromString $ "54992efb-4738-4f00-9c69-979d28cee5ff"] ++
              [q4_ait1_q6_aYes_fuq4_ait_question1 ^. uuid]
        let aitPlainWithIds =
              AnswerItemTemplatePlainWithIds
              { _answerItemTemplatePlainWithIdsTitle = q4_ait ^. title
              , _answerItemTemplatePlainWithIdsQuestionIds = aitQuestionIds
              }
        let edited_e_km1_ch2_ansMaybe6_fuq4 =
              e_km1_ch2_ansMaybe6_fuq4 & answerItemTemplatePlainWithIds .~ ChangedValue (Just aitPlainWithIds)
        let reqState =
              createTestMigratorStateWithEvents [] [EditQuestionEvent' edited_e_km1_ch2_ansMaybe6_fuq4] (Just km1WithQ4)
          -- When:
        resState <- migrate reqState
          -- Then:
        let (ConflictState (CorrectorConflict (EditQuestionEvent' resEvent))) = resState ^. migrationState
        resEvent ^. uuid `shouldNotBe` e_km1_ch2_ansMaybe6_fuq4 ^. uuid
        let (ChangedValue (Just resAit)) = resEvent ^. answerItemTemplatePlainWithIds
        resAit ^. questionIds `shouldBe`
          [q4_ait1_q6_aYes_fuq4_ait_question2 ^. uuid, q4_ait1_q6_aYes_fuq4_ait_question1 ^. uuid]
    -- -------------------------------------------------------------
    -- -------------------------------------------------------------
    describe "Sanatize: EditQuestionEvent (AnswerItemTemplate)" $ do
      it "QType - QuestionTypeOptions: Event - all KM uuids exists, no new added in event" $
        -- Given:
       do
        let reqState = createTestMigratorStateWithEvents [] [EditQuestionEvent' e_km1_ch2_q4_ait1_q6] (Just km1WithQ4)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditQuestionEvent' resEvent))) = resState ^. migrationState
        resEvent ^. uuid `shouldNotBe` e_km1_ch2_q4_ait1_q6 ^. uuid
        resEvent ^. answerIds `shouldBe`
          (ChangedValue $ Just [q4_ait1_q6_answerYes ^. uuid, q4_ait1_q6_answerNo ^. uuid])
        resEvent ^. referenceIds `shouldBe` ChangedValue [referenceCh2 ^. uuid, referenceCh1 ^. uuid]
        resEvent ^. expertIds `shouldBe` ChangedValue [expertNikola ^. uuid, expertAlbert ^. uuid]
      it "QType - QuestionTypeOptions: Event - some KM uuids missing, no new added in event" $
        -- Given:
       do
        let qAnswerIds = Just [q4_ait1_q6_answerYes ^. uuid]
        let qReferenceIds = [referenceCh2 ^. uuid]
        let qExpertIds = [expertNikola ^. uuid]
        let edited_1_event = e_km1_ch2_q4_ait1_q6 & answerIds .~ ChangedValue qAnswerIds
        let edited_2_event = edited_1_event & referenceIds .~ ChangedValue qReferenceIds
        let edited_3_event = edited_2_event & expertIds .~ ChangedValue qExpertIds
        let reqState = createTestMigratorStateWithEvents [] [EditQuestionEvent' edited_3_event] (Just km1WithQ4)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditQuestionEvent' resEvent))) = resState ^. migrationState
        resEvent ^. uuid `shouldNotBe` e_km1_ch2_q4_ait1_q6 ^. uuid
        resEvent ^. answerIds `shouldBe`
          (ChangedValue $ Just [q4_ait1_q6_answerYes ^. uuid, q4_ait1_q6_answerNo ^. uuid])
        resEvent ^. referenceIds `shouldBe` ChangedValue [referenceCh2 ^. uuid, referenceCh1 ^. uuid]
        resEvent ^. expertIds `shouldBe` ChangedValue [expertNikola ^. uuid, expertAlbert ^. uuid]
      it "QType - QuestionTypeOptions: Event - all KM uuids exists, new added in event but without existing in KM" $
        -- Given:
       do
        let qAnswerIds =
              Just $
              [q4_ait1_q6_answerYes ^. uuid] ++
              [fromJust . U.fromString $ "54992efb-4738-4f00-9c69-979d28cee5ff"] ++
              [q2_aYes_fuq1_aYes_fuq2_answerNo ^. uuid]
        let qReferenceIds =
              [referenceCh2 ^. uuid] ++
              [fromJust . U.fromString $ "bdbd95fd-8ea5-485d-9486-ef452b0a661e"] ++ [referenceCh1 ^. uuid]
        let qExpertIds =
              [expertNikola ^. uuid] ++
              [fromJust . U.fromString $ "e47df67f-7e6d-4e0f-950d-5035a48087a0"] ++ [expertAlbert ^. uuid]
        let edited_1_event = e_km1_ch2_q4_ait1_q6 & answerIds .~ ChangedValue qAnswerIds
        let edited_2_event = edited_1_event & referenceIds .~ ChangedValue qReferenceIds
        let edited_3_event = edited_2_event & expertIds .~ ChangedValue qExpertIds
        let reqState = createTestMigratorStateWithEvents [] [EditQuestionEvent' edited_3_event] (Just km1WithQ4)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditQuestionEvent' resEvent))) = resState ^. migrationState
        resEvent ^. uuid `shouldNotBe` e_km1_ch2_q4_ait1_q6 ^. uuid
        resEvent ^. answerIds `shouldBe`
          (ChangedValue $ Just [q4_ait1_q6_answerYes ^. uuid, q4_ait1_q6_answerNo ^. uuid])
        resEvent ^. referenceIds `shouldBe` ChangedValue [referenceCh2 ^. uuid, referenceCh1 ^. uuid]
        resEvent ^. expertIds `shouldBe` ChangedValue [expertNikola ^. uuid, expertAlbert ^. uuid]
      -- -------------------------------------------------------------
      it "QType - QuestionTypeList: Event - all KM uuids exists, no new added in event" $
        -- Given:
       do
        let reqState = createTestMigratorStateWithEvents [] [EditQuestionEvent' e_km1_ch2_q4_ait1_q5] (Just km1WithQ4)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditQuestionEvent' resEvent))) = resState ^. migrationState
        resEvent ^. uuid `shouldNotBe` e_km1_ch2_q4_ait1_q5 ^. uuid
        let (ChangedValue (Just resAit)) = resEvent ^. answerItemTemplatePlainWithIds
        resAit ^. questionIds `shouldBe` [q4_ait1_q5_ait2_question8 ^. uuid, q4_ait1_q5_ait2_question7 ^. uuid]
      it "QType - QuestionTypeList: Event - some KM uuids missing, no new added in event" $
        -- Given:
       do
        let aitQuestionIds = [q4_ait1_q5_ait2_question8 ^. uuid]
        let aitPlainWithIds =
              AnswerItemTemplatePlainWithIds
              { _answerItemTemplatePlainWithIdsTitle = q4_ait ^. title
              , _answerItemTemplatePlainWithIdsQuestionIds = aitQuestionIds
              }
        let edited_e_km1_ch2_q4_ait1_q5 =
              e_km1_ch2_q4_ait1_q5 & answerItemTemplatePlainWithIds .~ ChangedValue (Just aitPlainWithIds)
        let reqState =
              createTestMigratorStateWithEvents [] [EditQuestionEvent' edited_e_km1_ch2_q4_ait1_q5] (Just km1WithQ4)
           -- When:
        resState <- migrate reqState
           -- Then:
        let (ConflictState (CorrectorConflict (EditQuestionEvent' resEvent))) = resState ^. migrationState
        resEvent ^. uuid `shouldNotBe` e_km1_ch2_q4_ait1_q5 ^. uuid
        let (ChangedValue (Just resAit)) = resEvent ^. answerItemTemplatePlainWithIds
        resAit ^. questionIds `shouldBe` [q4_ait1_q5_ait2_question8 ^. uuid, q4_ait1_q5_ait2_question7 ^. uuid]
      it "QType - QuestionTypeList: Event - all KM uuids exists, new added in event but without existing in KM" $
          -- Given:
       do
        let aitQuestionIds =
              [q4_ait1_q5_ait2_question8 ^. uuid] ++
              [fromJust . U.fromString $ "54992efb-4738-4f00-9c69-979d28cee5ff"] ++ [q4_ait1_q5_ait2_question7 ^. uuid]
        let aitPlainWithIds =
              AnswerItemTemplatePlainWithIds
              { _answerItemTemplatePlainWithIdsTitle = q4_ait ^. title
              , _answerItemTemplatePlainWithIdsQuestionIds = aitQuestionIds
              }
        let edited_e_km1_ch2_q4_ait1_q5 =
              e_km1_ch2_q4_ait1_q5 & answerItemTemplatePlainWithIds .~ ChangedValue (Just aitPlainWithIds)
        let reqState =
              createTestMigratorStateWithEvents [] [EditQuestionEvent' edited_e_km1_ch2_q4_ait1_q5] (Just km1WithQ4)
          -- When:
        resState <- migrate reqState
          -- Then:
        let (ConflictState (CorrectorConflict (EditQuestionEvent' resEvent))) = resState ^. migrationState
        resEvent ^. uuid `shouldNotBe` e_km1_ch2_q4_ait1_q5 ^. uuid
        let (ChangedValue (Just resAit)) = resEvent ^. answerItemTemplatePlainWithIds
        resAit ^. questionIds `shouldBe` [q4_ait1_q5_ait2_question8 ^. uuid, q4_ait1_q5_ait2_question7 ^. uuid]
