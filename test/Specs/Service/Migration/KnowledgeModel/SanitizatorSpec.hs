module Specs.Service.Migration.KnowledgeModel.SanitizatorSpec where

import Control.Lens
import Data.Maybe
import qualified Data.UUID as U
import Test.Hspec hiding (shouldBe, shouldNotBe)
import Test.Hspec.Expectations.Pretty

import Database.Migration.Development.Event.Data.Events
import Database.Migration.Development.KnowledgeModel.Data.AnswersAndFollowUpQuestions
import Database.Migration.Development.KnowledgeModel.Data.Chapters
import Database.Migration.Development.KnowledgeModel.Data.Experts
import Database.Migration.Development.KnowledgeModel.Data.Integrations
import Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Database.Migration.Development.KnowledgeModel.Data.Questions
import Database.Migration.Development.KnowledgeModel.Data.References
import Database.Migration.Development.KnowledgeModel.Data.Tags
import LensesConfig
import Model.Event.Event
import Model.Event.EventField
import Model.Event.Question.QuestionEvent
import Model.KnowledgeModel.KnowledgeModel
import Model.Migrator.MigratorState
import Service.Migration.KnowledgeModel.Migrator

import Specs.Service.Migration.KnowledgeModel.Common

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
        resEvent ^. chapterUuids `shouldBe` ChangedValue [chapter3 ^. uuid, chapter2 ^. uuid, chapter1 ^. uuid]
        resEvent ^. tagUuids `shouldBe` ChangedValue [tagBioInformatic ^. uuid, tagDataScience ^. uuid]
        resEvent ^. integrationUuids `shouldBe` ChangedValue [bioPortal ^. uuid, ontologyPortal ^. uuid]
      it "Event - some KM uuids missing, no new added in event" $
        -- Given:
       do
        let kmChapterUuids = _chapterUuid <$> [chapter3, chapter2]
        let kmTagUuids = _tagUuid <$> [tagBioInformatic]
        let kmIntegrationUuids = _integrationUuid <$> [bioPortal]
        let edited_e_km1 =
              ((e_km1 & chapterUuids .~ ChangedValue kmChapterUuids) & tagUuids .~ ChangedValue kmTagUuids) &
              integrationUuids .~
              ChangedValue kmIntegrationUuids
        let reqState = createTestMigratorStateWithEvents [] [EditKnowledgeModelEvent' edited_e_km1] (Just km1)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditKnowledgeModelEvent' resEvent))) = resState ^. migrationState
        resEvent ^. uuid `shouldNotBe` e_km1 ^. uuid
        resEvent ^. chapterUuids `shouldBe` ChangedValue [chapter3 ^. uuid, chapter2 ^. uuid, chapter1 ^. uuid]
        resEvent ^. tagUuids `shouldBe` ChangedValue [tagBioInformatic ^. uuid, tagDataScience ^. uuid]
        resEvent ^. integrationUuids `shouldBe` ChangedValue [bioPortal ^. uuid, ontologyPortal ^. uuid]
      it "Event - all KM uuids exists, new added in event but without existing in KM" $
        -- Given:
       do
        let kmChapterUuids =
              [chapter3 ^. uuid] ++
              [chapter2 ^. uuid] ++
              [fromJust . U.fromString $ "54992efb-4738-4f00-9c69-979d28cee5ff"] ++ [chapter1 ^. uuid]
        let kmTagUuids =
              [tagBioInformatic ^. uuid] ++
              [fromJust . U.fromString $ "b28d289b-e373-49a2-9c91-b153cb62d894"] ++ [tagDataScience ^. uuid]
        let kmIntegrationUuids =
              [bioPortal ^. uuid] ++
              [fromJust . U.fromString $ "eb75a1a7-2760-446a-9a44-17b8f38679bf"] ++ [ontologyPortal ^. uuid]
        let edited_e_km1 =
              ((e_km1 & chapterUuids .~ ChangedValue kmChapterUuids) & tagUuids .~ ChangedValue kmTagUuids) &
              integrationUuids .~
              ChangedValue kmIntegrationUuids
        let reqState = createTestMigratorStateWithEvents [] [EditKnowledgeModelEvent' edited_e_km1] (Just km1)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditKnowledgeModelEvent' resEvent))) = resState ^. migrationState
        resEvent ^. uuid `shouldNotBe` e_km1 ^. uuid
        resEvent ^. chapterUuids `shouldBe` ChangedValue [chapter3 ^. uuid, chapter2 ^. uuid, chapter1 ^. uuid]
        resEvent ^. tagUuids `shouldBe` ChangedValue [tagBioInformatic ^. uuid, tagDataScience ^. uuid]
        resEvent ^. integrationUuids `shouldBe` ChangedValue [bioPortal ^. uuid, ontologyPortal ^. uuid]
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
        resEvent ^. questionUuids `shouldBe` ChangedValue [question2 ^. uuid, question1 ^. uuid]
      it "Event - some KM uuids missing, no new added in event" $
        -- Given:
       do
        let chQuestionUuids = [question2 ^. uuid]
        let edited_e_km1_ch1 = e_km1_ch1 & questionUuids .~ ChangedValue chQuestionUuids
        let reqState = createTestMigratorStateWithEvents [] [EditChapterEvent' edited_e_km1_ch1] (Just km1)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditChapterEvent' resEvent))) = resState ^. migrationState
        resEvent ^. uuid `shouldNotBe` e_km1_ch1 ^. uuid
        resEvent ^. questionUuids `shouldBe` ChangedValue [question2 ^. uuid, question1 ^. uuid]
      it "Event - all KM uuids exists, new added in event but without existing in KM" $
        -- Given:
       do
        let chQuestionUuids =
              [question2 ^. uuid] ++
              [fromJust . U.fromString $ "54992efb-4738-4f00-9c69-979d28cee5ff"] ++ [question1 ^. uuid]
        let edited_e_km1_ch1 = e_km1_ch1 & questionUuids .~ ChangedValue chQuestionUuids
        let reqState = createTestMigratorStateWithEvents [] [EditChapterEvent' edited_e_km1_ch1] (Just km1)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditChapterEvent' resEvent))) = resState ^. migrationState
        resEvent ^. uuid `shouldNotBe` e_km1_ch1 ^. uuid
        resEvent ^. questionUuids `shouldBe` ChangedValue [question2 ^. uuid, question1 ^. uuid]
    -- -------------------------------------------------------------
    -- -------------------------------------------------------------
    describe "Sanatize: EditQuestionEvent" $ do
      it "QType - QuestionTypeOptions: Event - all KM uuids exists, no new added in event" $
        -- Given:
       do
        let reqState =
              createTestMigratorStateWithEvents
                []
                [EditQuestionEvent' (EditOptionsQuestionEvent' e_km1_ch1_q2)]
                (Just km1)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditQuestionEvent' (EditOptionsQuestionEvent' resEvent)))) =
              resState ^. migrationState
        resEvent ^. uuid `shouldNotBe` e_km1_ch1_q2 ^. uuid
        resEvent ^. answerUuids `shouldBe` (ChangedValue $ [q2_answerYes ^. uuid, q2_answerNo ^. uuid])
        resEvent ^. referenceUuids `shouldBe` ChangedValue [referenceCh2 ^. uuid, referenceCh1 ^. uuid]
        resEvent ^. expertUuids `shouldBe` ChangedValue [expertNikola ^. uuid, expertAlbert ^. uuid]
      it "QType - QuestionTypeOptions: Event - some KM uuids missing, no new added in event" $
        -- Given:
       do
        let qAnswerUuids = [q2_answerYes ^. uuid]
        let qReferenceUuids = [referenceCh2 ^. uuid]
        let qExpertUuids = [expertNikola ^. uuid]
        let edited_1_e_km1_ch1_q2 = e_km1_ch1_q2 & answerUuids .~ ChangedValue qAnswerUuids
        let edited_2_e_km1_ch1_q2 = edited_1_e_km1_ch1_q2 & referenceUuids .~ ChangedValue qReferenceUuids
        let edited_3_e_km1_ch1_q2 = edited_2_e_km1_ch1_q2 & expertUuids .~ ChangedValue qExpertUuids
        let reqState =
              createTestMigratorStateWithEvents
                []
                [EditQuestionEvent' (EditOptionsQuestionEvent' edited_3_e_km1_ch1_q2)]
                (Just km1)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditQuestionEvent' (EditOptionsQuestionEvent' resEvent)))) =
              resState ^. migrationState
        resEvent ^. uuid `shouldNotBe` e_km1_ch1_q2 ^. uuid
        resEvent ^. answerUuids `shouldBe` (ChangedValue $ [q2_answerYes ^. uuid, q2_answerNo ^. uuid])
        resEvent ^. referenceUuids `shouldBe` ChangedValue [referenceCh2 ^. uuid, referenceCh1 ^. uuid]
        resEvent ^. expertUuids `shouldBe` ChangedValue [expertNikola ^. uuid, expertAlbert ^. uuid]
      it "QType - QuestionTypeOptions: Event - all KM uuids exists, new added in event but without existing in KM" $
          -- Given:
       do
        let qAnswerUuids =
              [q2_answerYes ^. uuid] ++
              [fromJust . U.fromString $ "54992efb-4738-4f00-9c69-979d28cee5ff"] ++ [q2_answerNo ^. uuid]
        let qReferenceUuids =
              [referenceCh2 ^. uuid] ++
              [fromJust . U.fromString $ "bdbd95fd-8ea5-485d-9486-ef452b0a661e"] ++ [referenceCh1 ^. uuid]
        let qExpertUuids =
              [expertNikola ^. uuid] ++
              [fromJust . U.fromString $ "e47df67f-7e6d-4e0f-950d-5035a48087a0"] ++ [expertAlbert ^. uuid]
        let edited_1_e_km1_ch1_q2 = e_km1_ch1_q2 & answerUuids .~ ChangedValue qAnswerUuids
        let edited_2_e_km1_ch1_q2 = edited_1_e_km1_ch1_q2 & referenceUuids .~ ChangedValue qReferenceUuids
        let edited_3_e_km1_ch1_q2 = edited_2_e_km1_ch1_q2 & expertUuids .~ ChangedValue qExpertUuids
        let reqState =
              createTestMigratorStateWithEvents
                []
                [EditQuestionEvent' (EditOptionsQuestionEvent' edited_3_e_km1_ch1_q2)]
                (Just km1)
          -- When:
        resState <- migrate reqState
          -- Then:
        let (ConflictState (CorrectorConflict (EditQuestionEvent' (EditOptionsQuestionEvent' resEvent)))) =
              resState ^. migrationState
        resEvent ^. uuid `shouldNotBe` e_km1_ch1_q2 ^. uuid
        resEvent ^. answerUuids `shouldBe` (ChangedValue $ [q2_answerYes ^. uuid, q2_answerNo ^. uuid])
        resEvent ^. referenceUuids `shouldBe` ChangedValue [referenceCh2 ^. uuid, referenceCh1 ^. uuid]
        resEvent ^. expertUuids `shouldBe` ChangedValue [expertNikola ^. uuid, expertAlbert ^. uuid]
      -- -------------------------------------------------------------
      it "QType - QuestionTypeList: Event - all KM uuids exists, no new added in event" $
        -- Given:
       do
        let reqState =
              createTestMigratorStateWithEvents
                []
                [EditQuestionEvent' (EditListQuestionEvent' e_km1_ch2_q4)]
                (Just km1WithQ4)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditQuestionEvent' (EditListQuestionEvent' resEvent)))) =
              resState ^. migrationState
        resEvent ^. uuid `shouldNotBe` e_km1_ch1_q2 ^. uuid
        let (ChangedValue resItqus) = resEvent ^. itemTemplateQuestionUuids
        resItqus `shouldBe` [q4_it1_question6 ^. uuid, q4_it1_question5 ^. uuid]
      it "QType - QuestionTypeList: Event - some KM uuids missing, no new added in event" $
        -- Given:
       do
        let itQuestionUuids = [q4_it1_question6 ^. uuid]
        let edited_1_e_km1_ch2_q4 = e_km1_ch2_q4 & itemTemplateQuestionUuids .~ ChangedValue itQuestionUuids
        let reqState =
              createTestMigratorStateWithEvents
                []
                [EditQuestionEvent' (EditListQuestionEvent' edited_1_e_km1_ch2_q4)]
                (Just km1WithQ4)
           -- When:
        resState <- migrate reqState
           -- Then:
        let (ConflictState (CorrectorConflict (EditQuestionEvent' (EditListQuestionEvent' resEvent)))) =
              resState ^. migrationState
        resEvent ^. uuid `shouldNotBe` e_km1_ch2_q4 ^. uuid
        let (ChangedValue resItqus) = resEvent ^. itemTemplateQuestionUuids
        resItqus `shouldBe` [q4_it1_question6 ^. uuid, q4_it1_question5 ^. uuid]
      it "QType - QuestionTypeList: Event - all KM uuids exists, new added in event but without existing in KM" $
          -- Given:
       do
        let itQuestionUuids =
              [q4_it1_question6 ^. uuid] ++
              [fromJust . U.fromString $ "54992efb-4738-4f00-9c69-979d28cee5ff"] ++ [q4_it1_question5 ^. uuid]
        let edited_1_e_km1_ch2_q4 = e_km1_ch2_q4 & itemTemplateQuestionUuids .~ ChangedValue itQuestionUuids
        let reqState =
              createTestMigratorStateWithEvents
                []
                [EditQuestionEvent' (EditListQuestionEvent' edited_1_e_km1_ch2_q4)]
                (Just km1WithQ4)
          -- When:
        resState <- migrate reqState
          -- Then:
        let (ConflictState (CorrectorConflict (EditQuestionEvent' (EditListQuestionEvent' resEvent)))) =
              resState ^. migrationState
        resEvent ^. uuid `shouldNotBe` e_km1_ch2_q4 ^. uuid
        let (ChangedValue resItqus) = resEvent ^. itemTemplateQuestionUuids
        resItqus `shouldBe` [q4_it1_question6 ^. uuid, q4_it1_question5 ^. uuid]
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
        resEvent ^. followUpUuids `shouldBe` ChangedValue [q2_aYes_fuQuestion1 ^. uuid]
      it "Event - some KM uuids missing, no new added in event" $
        -- Given:
       do
        let ansFollowUpUuids = []
        let edited_e_km1_ch1_q2_aYes1_2 = e_km1_ch1_q2_aYes1_2 & followUpUuids .~ ChangedValue ansFollowUpUuids
        let reqState = createTestMigratorStateWithEvents [] [EditAnswerEvent' edited_e_km1_ch1_q2_aYes1_2] (Just km1)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditAnswerEvent' resEvent))) = resState ^. migrationState
        resEvent ^. uuid `shouldNotBe` e_km1_ch1_q2_aYes1_2 ^. uuid
        resEvent ^. followUpUuids `shouldBe` ChangedValue [q2_aYes_fuQuestion1 ^. uuid]
      it "Event - all KM uuids exists, new added in event but without existing in KM" $
        -- Given:
       do
        let ansFollowUpUuids =
              [fromJust . U.fromString $ "54992efb-4738-4f00-9c69-979d28cee5ff"] ++
              [q2_aYes_fuQuestion1 ^. uuid] ++ [fromJust . U.fromString $ "3534d39b-e493-4e9f-b84d-f302c4077b5c"]
        let edited_e_km1_ch1_q2_aYes1_2 = e_km1_ch1_q2_aYes1_2 & followUpUuids .~ ChangedValue ansFollowUpUuids
        let reqState = createTestMigratorStateWithEvents [] [EditAnswerEvent' edited_e_km1_ch1_q2_aYes1_2] (Just km1)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditAnswerEvent' resEvent))) = resState ^. migrationState
        resEvent ^. uuid `shouldNotBe` e_km1_ch1_q2_aYes1_2 ^. uuid
        resEvent ^. followUpUuids `shouldBe` ChangedValue [q2_aYes_fuQuestion1 ^. uuid]
    -- -------------------------------------------------------------
    -- -------------------------------------------------------------
    describe "Sanatize: EditQuestionEvent (FollowUps)" $ do
      it "QType - QuestionTypeOptions: Event - all KM uuids exists, no new added in event" $
        -- Given:
       do
        let reqState =
              createTestMigratorStateWithEvents
                []
                [EditQuestionEvent' (EditOptionsQuestionEvent' e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2)]
                (Just km1)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditQuestionEvent' (EditOptionsQuestionEvent' resEvent)))) =
              resState ^. migrationState
        resEvent ^. uuid `shouldNotBe` e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2 ^. uuid
        resEvent ^. answerUuids `shouldBe`
          (ChangedValue $ [q2_aYes_fuq1_aYes_fuq2_answerYes ^. uuid, q2_aYes_fuq1_aYes_fuq2_answerNo ^. uuid])
        resEvent ^. referenceUuids `shouldBe` ChangedValue []
        resEvent ^. expertUuids `shouldBe` ChangedValue []
      it "QType - QuestionTypeOptions: Event - some KM uuids missing, no new added in event" $
        -- Given:
       do
        let qAnswerUuids = [q2_aYes_fuq1_aYes_fuq2_answerYes ^. uuid]
        let qReferenceUuids = [referenceCh2 ^. uuid]
        let qExpertUuids = [expertNikola ^. uuid]
        let edited_1_event = e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2 & answerUuids .~ ChangedValue qAnswerUuids
        let edited_2_event = edited_1_event & referenceUuids .~ ChangedValue qReferenceUuids
        let edited_3_event = edited_2_event & expertUuids .~ ChangedValue qExpertUuids
        let reqState =
              createTestMigratorStateWithEvents
                []
                [EditQuestionEvent' (EditOptionsQuestionEvent' edited_3_event)]
                (Just km1)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditQuestionEvent' (EditOptionsQuestionEvent' resEvent)))) =
              resState ^. migrationState
        resEvent ^. uuid `shouldNotBe` e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2 ^. uuid
        resEvent ^. answerUuids `shouldBe`
          (ChangedValue $ [q2_aYes_fuq1_aYes_fuq2_answerYes ^. uuid, q2_aYes_fuq1_aYes_fuq2_answerNo ^. uuid])
        resEvent ^. referenceUuids `shouldBe` ChangedValue []
        resEvent ^. expertUuids `shouldBe` ChangedValue []
      it "QType - QuestionTypeOptions: Event - all KM uuids exists, new added in event but without existing in KM" $
        -- Given:
       do
        let qAnswerUuids =
              [q2_aYes_fuq1_aYes_fuq2_answerYes ^. uuid] ++
              [fromJust . U.fromString $ "54992efb-4738-4f00-9c69-979d28cee5ff"] ++
              [q2_aYes_fuq1_aYes_fuq2_answerNo ^. uuid]
        let qReferenceUuids =
              [referenceCh2 ^. uuid] ++
              [fromJust . U.fromString $ "bdbd95fd-8ea5-485d-9486-ef452b0a661e"] ++ [referenceCh1 ^. uuid]
        let qExpertUuids =
              [expertNikola ^. uuid] ++
              [fromJust . U.fromString $ "e47df67f-7e6d-4e0f-950d-5035a48087a0"] ++ [expertAlbert ^. uuid]
        let edited_1_event = e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2 & answerUuids .~ ChangedValue qAnswerUuids
        let edited_2_event = edited_1_event & referenceUuids .~ ChangedValue qReferenceUuids
        let edited_3_event = edited_2_event & expertUuids .~ ChangedValue qExpertUuids
        let reqState =
              createTestMigratorStateWithEvents
                []
                [EditQuestionEvent' (EditOptionsQuestionEvent' edited_3_event)]
                (Just km1)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditQuestionEvent' (EditOptionsQuestionEvent' resEvent)))) =
              resState ^. migrationState
        resEvent ^. uuid `shouldNotBe` e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2 ^. uuid
        resEvent ^. answerUuids `shouldBe`
          (ChangedValue $ [q2_aYes_fuq1_aYes_fuq2_answerYes ^. uuid, q2_aYes_fuq1_aYes_fuq2_answerNo ^. uuid])
        resEvent ^. referenceUuids `shouldBe` ChangedValue []
        resEvent ^. expertUuids `shouldBe` ChangedValue []
      -- -------------------------------------------------------------
      it "QType - QuestionTypeList: Event - all KM uuids exists, no new added in event" $
        -- Given:
       do
        let reqState =
              createTestMigratorStateWithEvents
                []
                [EditQuestionEvent' (EditListQuestionEvent' e_km1_ch2_ansMaybe6_fuq4)]
                (Just km1WithQ4)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditQuestionEvent' (EditListQuestionEvent' resEvent)))) =
              resState ^. migrationState
        resEvent ^. uuid `shouldNotBe` e_km1_ch2_ansMaybe6_fuq4 ^. uuid
        let (ChangedValue resItqus) = resEvent ^. itemTemplateQuestionUuids
        resItqus `shouldBe` [q4_it1_q6_aYes_fuq4_it_question2 ^. uuid, q4_it1_q6_aYes_fuq4_it_question1 ^. uuid]
      it "QType - QuestionTypeList: Event - some KM uuids missing, no new added in event" $
        -- Given:
       do
        let itQuestionUuids = [q4_it1_q6_aYes_fuq4_it_question2 ^. uuid]
        let edited_e_km1_ch2_ansMaybe6_fuq4 =
              e_km1_ch2_ansMaybe6_fuq4 & itemTemplateQuestionUuids .~ ChangedValue itQuestionUuids
        let reqState =
              createTestMigratorStateWithEvents
                []
                [EditQuestionEvent' (EditListQuestionEvent' edited_e_km1_ch2_ansMaybe6_fuq4)]
                (Just km1WithQ4)
           -- When:
        resState <- migrate reqState
           -- Then:
        let (ConflictState (CorrectorConflict (EditQuestionEvent' (EditListQuestionEvent' resEvent)))) =
              resState ^. migrationState
        resEvent ^. uuid `shouldNotBe` e_km1_ch2_ansMaybe6_fuq4 ^. uuid
        let (ChangedValue resItqus) = resEvent ^. itemTemplateQuestionUuids
        resItqus `shouldBe` [q4_it1_q6_aYes_fuq4_it_question2 ^. uuid, q4_it1_q6_aYes_fuq4_it_question1 ^. uuid]
      it "QType - QuestionTypeList: Event - all KM uuids exists, new added in event but without existing in KM" $
          -- Given:
       do
        let itQuestionUuids =
              [q4_it1_q6_aYes_fuq4_it_question2 ^. uuid] ++
              [fromJust . U.fromString $ "54992efb-4738-4f00-9c69-979d28cee5ff"] ++
              [q4_it1_q6_aYes_fuq4_it_question1 ^. uuid]
        let edited_e_km1_ch2_ansMaybe6_fuq4 =
              e_km1_ch2_ansMaybe6_fuq4 & itemTemplateQuestionUuids .~ ChangedValue itQuestionUuids
        let reqState =
              createTestMigratorStateWithEvents
                []
                [EditQuestionEvent' (EditListQuestionEvent' edited_e_km1_ch2_ansMaybe6_fuq4)]
                (Just km1WithQ4)
          -- When:
        resState <- migrate reqState
          -- Then:
        let (ConflictState (CorrectorConflict (EditQuestionEvent' (EditListQuestionEvent' resEvent)))) =
              resState ^. migrationState
        resEvent ^. uuid `shouldNotBe` e_km1_ch2_ansMaybe6_fuq4 ^. uuid
        let (ChangedValue resItqus) = resEvent ^. itemTemplateQuestionUuids
        resItqus `shouldBe` [q4_it1_q6_aYes_fuq4_it_question2 ^. uuid, q4_it1_q6_aYes_fuq4_it_question1 ^. uuid]
    -- -------------------------------------------------------------
    -- -------------------------------------------------------------
    describe "Sanatize: EditQuestionEvent (AnswerItemTemplate)" $ do
      it "QType - QuestionTypeOptions: Event - all KM uuids exists, no new added in event" $
        -- Given:
       do
        let reqState = createTestMigratorStateWithEvents [] [EditQuestionEvent' e_km1_ch2_q4_it1_q6'] (Just km1WithQ4)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditQuestionEvent' (EditOptionsQuestionEvent' resEvent)))) =
              resState ^. migrationState
        resEvent ^. uuid `shouldNotBe` e_km1_ch2_q4_it1_q6 ^. uuid
        resEvent ^. answerUuids `shouldBe` (ChangedValue $ [q4_it1_q6_answerYes ^. uuid, q4_it1_q6_answerNo ^. uuid])
        resEvent ^. referenceUuids `shouldBe` ChangedValue [referenceCh2 ^. uuid, referenceCh1 ^. uuid]
        resEvent ^. expertUuids `shouldBe` ChangedValue [expertNikola ^. uuid, expertAlbert ^. uuid]
      it "QType - QuestionTypeOptions: Event - some KM uuids missing, no new added in event" $
        -- Given:
       do
        let qAnswerUuids = [q4_it1_q6_answerYes ^. uuid]
        let qReferenceUuids = [referenceCh2 ^. uuid]
        let qExpertUuids = [expertNikola ^. uuid]
        let edited_1_event = e_km1_ch2_q4_it1_q6 & answerUuids .~ ChangedValue qAnswerUuids
        let edited_2_event = edited_1_event & referenceUuids .~ ChangedValue qReferenceUuids
        let edited_3_event = edited_2_event & expertUuids .~ ChangedValue qExpertUuids
        let reqState =
              createTestMigratorStateWithEvents
                []
                [EditQuestionEvent' (EditOptionsQuestionEvent' edited_3_event)]
                (Just km1WithQ4)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditQuestionEvent' (EditOptionsQuestionEvent' resEvent)))) =
              resState ^. migrationState
        resEvent ^. uuid `shouldNotBe` e_km1_ch2_q4_it1_q6 ^. uuid
        resEvent ^. answerUuids `shouldBe` (ChangedValue $ [q4_it1_q6_answerYes ^. uuid, q4_it1_q6_answerNo ^. uuid])
        resEvent ^. referenceUuids `shouldBe` ChangedValue [referenceCh2 ^. uuid, referenceCh1 ^. uuid]
        resEvent ^. expertUuids `shouldBe` ChangedValue [expertNikola ^. uuid, expertAlbert ^. uuid]
      it "QType - QuestionTypeOptions: Event - all KM uuids exists, new added in event but without existing in KM" $
        -- Given:
       do
        let qAnswerUuids =
              [q4_it1_q6_answerYes ^. uuid] ++
              [fromJust . U.fromString $ "54992efb-4738-4f00-9c69-979d28cee5ff"] ++
              [q2_aYes_fuq1_aYes_fuq2_answerNo ^. uuid]
        let qReferenceUuids =
              [referenceCh2 ^. uuid] ++
              [fromJust . U.fromString $ "bdbd95fd-8ea5-485d-9486-ef452b0a661e"] ++ [referenceCh1 ^. uuid]
        let qExpertUuids =
              [expertNikola ^. uuid] ++
              [fromJust . U.fromString $ "e47df67f-7e6d-4e0f-950d-5035a48087a0"] ++ [expertAlbert ^. uuid]
        let edited_1_event = e_km1_ch2_q4_it1_q6 & answerUuids .~ ChangedValue qAnswerUuids
        let edited_2_event = edited_1_event & referenceUuids .~ ChangedValue qReferenceUuids
        let edited_3_event = edited_2_event & expertUuids .~ ChangedValue qExpertUuids
        let reqState =
              createTestMigratorStateWithEvents
                []
                [EditQuestionEvent' (EditOptionsQuestionEvent' edited_3_event)]
                (Just km1WithQ4)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditQuestionEvent' (EditOptionsQuestionEvent' resEvent)))) =
              resState ^. migrationState
        resEvent ^. uuid `shouldNotBe` e_km1_ch2_q4_it1_q6 ^. uuid
        resEvent ^. answerUuids `shouldBe` (ChangedValue $ [q4_it1_q6_answerYes ^. uuid, q4_it1_q6_answerNo ^. uuid])
        resEvent ^. referenceUuids `shouldBe` ChangedValue [referenceCh2 ^. uuid, referenceCh1 ^. uuid]
        resEvent ^. expertUuids `shouldBe` ChangedValue [expertNikola ^. uuid, expertAlbert ^. uuid]
      -- -------------------------------------------------------------
      it "QType - QuestionTypeList: Event - all KM uuids exists, no new added in event" $
        -- Given:
       do
        let reqState = createTestMigratorStateWithEvents [] [EditQuestionEvent' e_km1_ch2_q4_it1_q5'] (Just km1WithQ4)
        -- When:
        resState <- migrate reqState
        -- Then:
        let (ConflictState (CorrectorConflict (EditQuestionEvent' (EditListQuestionEvent' resEvent)))) =
              resState ^. migrationState
        resEvent ^. uuid `shouldNotBe` e_km1_ch2_q4_it1_q5 ^. uuid
        let (ChangedValue resItqus) = resEvent ^. itemTemplateQuestionUuids
        resItqus `shouldBe` [q4_it1_q5_it2_question8 ^. uuid, q4_it1_q5_it2_question7 ^. uuid]
      it "QType - QuestionTypeList: Event - some KM uuids missing, no new added in event" $
        -- Given:
       do
        let itQuestionUuids = [q4_it1_q5_it2_question8 ^. uuid]
        let edited_e_km1_ch2_q4_it1_q5 = e_km1_ch2_q4_it1_q5 & itemTemplateQuestionUuids .~ ChangedValue itQuestionUuids
        let reqState =
              createTestMigratorStateWithEvents
                []
                [EditQuestionEvent' (EditListQuestionEvent' edited_e_km1_ch2_q4_it1_q5)]
                (Just km1WithQ4)
           -- When:
        resState <- migrate reqState
           -- Then:
        let (ConflictState (CorrectorConflict (EditQuestionEvent' (EditListQuestionEvent' resEvent)))) =
              resState ^. migrationState
        resEvent ^. uuid `shouldNotBe` e_km1_ch2_q4_it1_q5 ^. uuid
        let (ChangedValue resItqus) = resEvent ^. itemTemplateQuestionUuids
        resItqus `shouldBe` [q4_it1_q5_it2_question8 ^. uuid, q4_it1_q5_it2_question7 ^. uuid]
      it "QType - QuestionTypeList: Event - all KM uuids exists, new added in event but without existing in KM" $
          -- Given:
       do
        let itQuestionUuids =
              [q4_it1_q5_it2_question8 ^. uuid] ++
              [fromJust . U.fromString $ "54992efb-4738-4f00-9c69-979d28cee5ff"] ++ [q4_it1_q5_it2_question7 ^. uuid]
        let edited_e_km1_ch2_q4_it1_q5 = e_km1_ch2_q4_it1_q5 & itemTemplateQuestionUuids .~ ChangedValue itQuestionUuids
        let reqState =
              createTestMigratorStateWithEvents
                []
                [EditQuestionEvent' . EditListQuestionEvent' $ edited_e_km1_ch2_q4_it1_q5]
                (Just km1WithQ4)
          -- When:
        resState <- migrate reqState
          -- Then:
        let (ConflictState (CorrectorConflict (EditQuestionEvent' (EditListQuestionEvent' resEvent)))) =
              resState ^. migrationState
        resEvent ^. uuid `shouldNotBe` e_km1_ch2_q4_it1_q5 ^. uuid
        let (ChangedValue resItqus) = resEvent ^. itemTemplateQuestionUuids
        resItqus `shouldBe` [q4_it1_q5_it2_question8 ^. uuid, q4_it1_q5_it2_question7 ^. uuid]
