module Wizard.Specs.Service.Migration.KnowledgeModel.Migrator.SanitizatorSpec where

import Data.Maybe
import qualified Data.UUID as U
import Test.Hspec hiding (shouldBe, shouldNotBe)
import Test.Hspec.Expectations.Pretty

import Wizard.Model.Migration.KnowledgeModel.MigratorState
import Wizard.Service.Migration.KnowledgeModel.Migrator.Migrator
import WizardLib.KnowledgeModel.Database.Migration.Development.Event.Data.Events
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.AnswersAndFollowUpQuestions
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Chapters
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Experts
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Integrations
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Questions
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.References
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Tags
import WizardLib.KnowledgeModel.Model.Event.Answer.AnswerEvent
import WizardLib.KnowledgeModel.Model.Event.Chapter.ChapterEvent
import WizardLib.KnowledgeModel.Model.Event.Event
import WizardLib.KnowledgeModel.Model.Event.EventField
import WizardLib.KnowledgeModel.Model.Event.KnowledgeModel.KnowledgeModelEvent
import WizardLib.KnowledgeModel.Model.Event.Question.QuestionEvent
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel

import Wizard.Specs.Common
import Wizard.Specs.Service.Migration.KnowledgeModel.Migrator.Common

sanitizatorSpec appContext =
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
            (Right resState) <- runInContext (migrate reqState) appContext
            -- Then:
            let (ConflictState (CorrectorConflict (Just (EditKnowledgeModelEvent' resEvent)))) = resState.migrationState
            resEvent.uuid `shouldBe` e_km1.uuid
            resEvent.chapterUuids `shouldBe` e_km1.chapterUuids
            resEvent.tagUuids `shouldBe` e_km1.tagUuids
            resEvent.integrationUuids `shouldBe` e_km1.integrationUuids
        it "Event - some KM uuids missing, no new added in event" $
          -- Given:
          do
            let kmChapterUuids = fmap (.uuid) [chapter3, chapter2]
            let kmTagUuids = (.uuid) <$> [tagBioInformatic]
            let kmIntegrationUuids = [widgetPortal.uuid, bioPortal.uuid]
            let edited_e_km1 =
                  e_km1
                    { chapterUuids = ChangedValue kmChapterUuids
                    , tagUuids = ChangedValue kmTagUuids
                    , integrationUuids = ChangedValue kmIntegrationUuids
                    }
                  :: EditKnowledgeModelEvent
            let reqState = createTestMigratorStateWithEvents [] [EditKnowledgeModelEvent' edited_e_km1] (Just km1)
            -- When:
            (Right resState) <- runInContext (migrate reqState) appContext
            -- Then:
            let (ConflictState (CorrectorConflict (Just (EditKnowledgeModelEvent' resEvent)))) = resState.migrationState
            resEvent.uuid `shouldNotBe` e_km1.uuid
            resEvent.chapterUuids `shouldBe` ChangedValue [chapter3.uuid, chapter2.uuid, chapter1.uuid]
            resEvent.tagUuids `shouldBe` ChangedValue [tagBioInformatic.uuid, tagDataScience.uuid]
            resEvent.integrationUuids
              `shouldBe` ChangedValue [widgetPortal.uuid, bioPortal.uuid, ontologyPortal.uuid]
        it "Event - all KM uuids exists, new added in event but without existing in KM" $
          -- Given:
          do
            let kmChapterUuids =
                  [chapter3.uuid]
                    ++ [chapter2.uuid]
                    ++ [fromJust . U.fromString $ "54992efb-4738-4f00-9c69-979d28cee5ff"]
                    ++ [chapter1.uuid]
            let kmTagUuids =
                  [tagBioInformatic.uuid]
                    ++ [fromJust . U.fromString $ "b28d289b-e373-49a2-9c91-b153cb62d894"]
                    ++ [tagDataScience.uuid]
            let kmIntegrationUuids =
                  [bioPortal.uuid]
                    ++ [fromJust . U.fromString $ "eb75a1a7-2760-446a-9a44-17b8f38679bf"]
                    ++ [widgetPortal.uuid, ontologyPortal.uuid]
            let edited_e_km1 =
                  e_km1
                    { chapterUuids = ChangedValue kmChapterUuids
                    , tagUuids = ChangedValue kmTagUuids
                    , integrationUuids = ChangedValue kmIntegrationUuids
                    }
                  :: EditKnowledgeModelEvent
            let reqState = createTestMigratorStateWithEvents [] [EditKnowledgeModelEvent' edited_e_km1] (Just km1)
            -- When:
            (Right resState) <- runInContext (migrate reqState) appContext
            -- Then:
            let (ConflictState (CorrectorConflict (Just (EditKnowledgeModelEvent' resEvent)))) = resState.migrationState
            resEvent.uuid `shouldNotBe` e_km1.uuid
            resEvent.chapterUuids `shouldBe` ChangedValue [chapter3.uuid, chapter2.uuid, chapter1.uuid]
            resEvent.tagUuids `shouldBe` ChangedValue [tagBioInformatic.uuid, tagDataScience.uuid]
            resEvent.integrationUuids
              `shouldBe` ChangedValue [bioPortal.uuid, widgetPortal.uuid, ontologyPortal.uuid]
      -- -------------------------------------------------------------
      -- -------------------------------------------------------------
      describe "Sanatize: EditChapterEvent" $ do
        it "Event - all KM uuids exists, no new added in event" $
          -- Given:
          do
            let reqState = createTestMigratorStateWithEvents [] [EditChapterEvent' e_km1_ch1] (Just km1)
            -- When:
            (Right resState) <- runInContext (migrate reqState) appContext
            -- Then:
            let (ConflictState (CorrectorConflict (Just (EditChapterEvent' resEvent)))) = resState.migrationState
            resEvent.uuid `shouldBe` e_km1_ch1.uuid
            resEvent.questionUuids `shouldBe` e_km1_ch1.questionUuids
        it "Event - some KM uuids missing, no new added in event" $
          -- Given:
          do
            let chQuestionUuids = [question2.uuid]
            let edited_e_km1_ch1 = e_km1_ch1 {questionUuids = ChangedValue chQuestionUuids} :: EditChapterEvent
            let reqState = createTestMigratorStateWithEvents [] [EditChapterEvent' edited_e_km1_ch1] (Just km1)
            -- When:
            (Right resState) <- runInContext (migrate reqState) appContext
            -- Then:
            let (ConflictState (CorrectorConflict (Just (EditChapterEvent' resEvent)))) = resState.migrationState
            resEvent.uuid `shouldNotBe` e_km1_ch1.uuid
            resEvent.questionUuids `shouldBe` ChangedValue [question2.uuid, question1.uuid]
        it "Event - all KM uuids exists, new added in event but without existing in KM" $
          -- Given:
          do
            let chQuestionUuids =
                  [question2.uuid]
                    ++ [fromJust . U.fromString $ "54992efb-4738-4f00-9c69-979d28cee5ff"]
                    ++ [question1.uuid]
            let edited_e_km1_ch1 = e_km1_ch1 {questionUuids = ChangedValue chQuestionUuids} :: EditChapterEvent
            let reqState = createTestMigratorStateWithEvents [] [EditChapterEvent' edited_e_km1_ch1] (Just km1)
            -- When:
            (Right resState) <- runInContext (migrate reqState) appContext
            -- Then:
            let (ConflictState (CorrectorConflict (Just (EditChapterEvent' resEvent)))) = resState.migrationState
            resEvent.uuid `shouldNotBe` e_km1_ch1.uuid
            resEvent.questionUuids `shouldBe` ChangedValue [question2.uuid, question1.uuid]
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
            (Right resState) <- runInContext (migrate reqState) appContext
            -- Then:
            let (ConflictState (CorrectorConflict (Just (EditQuestionEvent' (EditOptionsQuestionEvent' resEvent))))) =
                  resState.migrationState
            resEvent.uuid `shouldBe` e_km1_ch1_q2.uuid
            resEvent.answerUuids `shouldBe` e_km1_ch1_q2.answerUuids
            resEvent.referenceUuids `shouldBe` e_km1_ch1_q2.referenceUuids
            resEvent.expertUuids `shouldBe` e_km1_ch1_q2.expertUuids
        it "QType - QuestionTypeOptions: Event - some KM uuids missing, no new added in event" $
          -- Given:
          do
            let qAnswerUuids = [q2_answerYes.uuid]
            let qReferenceUuids = [km1_ch1_q2_r2.uuid]
            let qExpertUuids = [km1_ch1_q2_eNikola.uuid]
            let edited_1_e_km1_ch1_q2 = e_km1_ch1_q2 {answerUuids = ChangedValue qAnswerUuids} :: EditOptionsQuestionEvent
            let edited_2_e_km1_ch1_q2 = edited_1_e_km1_ch1_q2 {referenceUuids = ChangedValue qReferenceUuids} :: EditOptionsQuestionEvent
            let edited_3_e_km1_ch1_q2 = edited_2_e_km1_ch1_q2 {expertUuids = ChangedValue qExpertUuids} :: EditOptionsQuestionEvent
            let reqState =
                  createTestMigratorStateWithEvents
                    []
                    [EditQuestionEvent' (EditOptionsQuestionEvent' edited_3_e_km1_ch1_q2)]
                    (Just km1)
            -- When:
            (Right resState) <- runInContext (migrate reqState) appContext
            -- Then:
            let (ConflictState (CorrectorConflict (Just (EditQuestionEvent' (EditOptionsQuestionEvent' resEvent))))) =
                  resState.migrationState
            resEvent.uuid `shouldNotBe` e_km1_ch1_q2.uuid
            resEvent.answerUuids `shouldBe` ChangedValue [q2_answerYes.uuid, q2_answerNo.uuid]
            resEvent.referenceUuids `shouldBe` ChangedValue [km1_ch1_q2_r2.uuid, km1_ch1_q2_r1.uuid]
            resEvent.expertUuids `shouldBe` ChangedValue [km1_ch1_q2_eNikola.uuid, km1_ch1_q2_eAlbert.uuid]
        it "QType - QuestionTypeOptions: Event - all KM uuids exists, new added in event but without existing in KM" $
          -- Given:
          do
            let qAnswerUuids =
                  [q2_answerYes.uuid]
                    ++ [fromJust . U.fromString $ "54992efb-4738-4f00-9c69-979d28cee5ff"]
                    ++ [q2_answerNo.uuid]
            let qReferenceUuids =
                  [km1_ch1_q2_r2.uuid]
                    ++ [fromJust . U.fromString $ "bdbd95fd-8ea5-485d-9486-ef452b0a661e"]
                    ++ [km1_ch1_q2_r1.uuid]
            let qExpertUuids =
                  [km1_ch1_q2_eNikola.uuid]
                    ++ [fromJust . U.fromString $ "e47df67f-7e6d-4e0f-950d-5035a48087a0"]
                    ++ [km1_ch1_q2_eAlbert.uuid]
            let edited_1_e_km1_ch1_q2 = e_km1_ch1_q2 {answerUuids = ChangedValue qAnswerUuids} :: EditOptionsQuestionEvent
            let edited_2_e_km1_ch1_q2 = edited_1_e_km1_ch1_q2 {referenceUuids = ChangedValue qReferenceUuids} :: EditOptionsQuestionEvent
            let edited_3_e_km1_ch1_q2 = edited_2_e_km1_ch1_q2 {expertUuids = ChangedValue qExpertUuids} :: EditOptionsQuestionEvent
            let reqState =
                  createTestMigratorStateWithEvents
                    []
                    [EditQuestionEvent' (EditOptionsQuestionEvent' edited_3_e_km1_ch1_q2)]
                    (Just km1)
            -- When:
            (Right resState) <- runInContext (migrate reqState) appContext
            -- Then:
            let (ConflictState (CorrectorConflict (Just (EditQuestionEvent' (EditOptionsQuestionEvent' resEvent))))) =
                  resState.migrationState
            resEvent.uuid `shouldNotBe` e_km1_ch1_q2.uuid
            resEvent.answerUuids `shouldBe` ChangedValue [q2_answerYes.uuid, q2_answerNo.uuid]
            resEvent.referenceUuids `shouldBe` ChangedValue [km1_ch1_q2_r2.uuid, km1_ch1_q2_r1.uuid]
            resEvent.expertUuids `shouldBe` ChangedValue [km1_ch1_q2_eNikola.uuid, km1_ch1_q2_eAlbert.uuid]
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
            (Right resState) <- runInContext (migrate reqState) appContext
            -- Then:
            let (ConflictState (CorrectorConflict (Just (EditQuestionEvent' (EditListQuestionEvent' resEvent))))) =
                  resState.migrationState
            resEvent.uuid `shouldNotBe` e_km1_ch1_q2.uuid
            let (ChangedValue resItqus) = resEvent.itemTemplateQuestionUuids
            resItqus `shouldBe` [q4_it1_question6.uuid, q4_it1_question5.uuid]
        it "QType - QuestionTypeList: Event - some KM uuids missing, no new added in event" $
          -- Given:
          do
            let itQuestionUuids = [q4_it1_question6.uuid]
            let edited_1_e_km1_ch2_q4 = e_km1_ch2_q4 {itemTemplateQuestionUuids = ChangedValue itQuestionUuids} :: EditListQuestionEvent
            let reqState =
                  createTestMigratorStateWithEvents
                    []
                    [EditQuestionEvent' (EditListQuestionEvent' edited_1_e_km1_ch2_q4)]
                    (Just km1WithQ4)
            -- When:
            (Right resState) <- runInContext (migrate reqState) appContext
            -- Then:
            let (ConflictState (CorrectorConflict (Just (EditQuestionEvent' (EditListQuestionEvent' resEvent))))) =
                  resState.migrationState
            resEvent.uuid `shouldNotBe` e_km1_ch2_q4.uuid
            let (ChangedValue resItqus) = resEvent.itemTemplateQuestionUuids
            resItqus `shouldBe` [q4_it1_question6.uuid, q4_it1_question5.uuid]
        it "QType - QuestionTypeList: Event - all KM uuids exists, new added in event but without existing in KM" $
          -- Given:
          do
            let itQuestionUuids =
                  [q4_it1_question6.uuid]
                    ++ [fromJust . U.fromString $ "54992efb-4738-4f00-9c69-979d28cee5ff"]
                    ++ [q4_it1_question5.uuid]
            let edited_1_e_km1_ch2_q4 = e_km1_ch2_q4 {itemTemplateQuestionUuids = ChangedValue itQuestionUuids} :: EditListQuestionEvent
            let reqState =
                  createTestMigratorStateWithEvents
                    []
                    [EditQuestionEvent' (EditListQuestionEvent' edited_1_e_km1_ch2_q4)]
                    (Just km1WithQ4)
            -- When:
            (Right resState) <- runInContext (migrate reqState) appContext
            -- Then:
            let (ConflictState (CorrectorConflict (Just (EditQuestionEvent' (EditListQuestionEvent' resEvent))))) =
                  resState.migrationState
            resEvent.uuid `shouldNotBe` e_km1_ch2_q4.uuid
            let (ChangedValue resItqus) = resEvent.itemTemplateQuestionUuids
            resItqus `shouldBe` [q4_it1_question6.uuid, q4_it1_question5.uuid]
      -- -------------------------------------------------------------
      -- -------------------------------------------------------------
      describe "Sanatize: EditAnswerEvent" $ do
        it "Event - all KM uuids exists, no new added in event" $
          -- Given:
          do
            let reqState = createTestMigratorStateWithEvents [] [EditAnswerEvent' e_km1_ch1_q2_aYes1_2] (Just km1)
            -- When:
            (Right resState) <- runInContext (migrate reqState) appContext
            -- Then:
            let (ConflictState (CorrectorConflict (Just (EditAnswerEvent' resEvent)))) = resState.migrationState
            resEvent.uuid `shouldBe` e_km1_ch1_q2_aYes1_2.uuid
            resEvent.followUpUuids `shouldBe` e_km1_ch1_q2_aYes1_2.followUpUuids
        it "Event - some KM uuids missing, no new added in event" $
          -- Given:
          do
            let ansFollowUpUuids = []
            let edited_e_km1_ch1_q2_aYes1_2 = e_km1_ch1_q2_aYes1_2 {followUpUuids = ChangedValue ansFollowUpUuids} :: EditAnswerEvent
            let reqState = createTestMigratorStateWithEvents [] [EditAnswerEvent' edited_e_km1_ch1_q2_aYes1_2] (Just km1)
            -- When:
            (Right resState) <- runInContext (migrate reqState) appContext
            -- Then:
            let (ConflictState (CorrectorConflict (Just (EditAnswerEvent' resEvent)))) = resState.migrationState
            resEvent.uuid `shouldNotBe` e_km1_ch1_q2_aYes1_2.uuid
            resEvent.followUpUuids `shouldBe` ChangedValue [q2_aYes_fuQuestion1.uuid]
        it "Event - all KM uuids exists, new added in event but without existing in KM" $
          -- Given:
          do
            let ansFollowUpUuids =
                  [fromJust . U.fromString $ "54992efb-4738-4f00-9c69-979d28cee5ff"]
                    ++ [q2_aYes_fuQuestion1.uuid]
                    ++ [fromJust . U.fromString $ "3534d39b-e493-4e9f-b84d-f302c4077b5c"]
            let edited_e_km1_ch1_q2_aYes1_2 = e_km1_ch1_q2_aYes1_2 {followUpUuids = ChangedValue ansFollowUpUuids} :: EditAnswerEvent
            let reqState = createTestMigratorStateWithEvents [] [EditAnswerEvent' edited_e_km1_ch1_q2_aYes1_2] (Just km1)
            -- When:
            (Right resState) <- runInContext (migrate reqState) appContext
            -- Then:
            let (ConflictState (CorrectorConflict (Just (EditAnswerEvent' resEvent)))) = resState.migrationState
            resEvent.uuid `shouldNotBe` e_km1_ch1_q2_aYes1_2.uuid
            resEvent.followUpUuids `shouldBe` ChangedValue [q2_aYes_fuQuestion1.uuid]
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
            (Right resState) <- runInContext (migrate reqState) appContext
            -- Then:
            let (ConflictState (CorrectorConflict (Just (EditQuestionEvent' (EditOptionsQuestionEvent' resEvent))))) =
                  resState.migrationState
            resEvent.uuid `shouldBe` e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2.uuid
            resEvent.answerUuids `shouldBe` e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2.answerUuids
            resEvent.referenceUuids `shouldBe` e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2.referenceUuids
            resEvent.expertUuids `shouldBe` e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2.expertUuids
        it "QType - QuestionTypeOptions: Event - some KM uuids missing, no new added in event" $
          -- Given:
          do
            let qAnswerUuids = [q2_aYes_fuq1_aYes_fuq2_answerYes.uuid]
            let qReferenceUuids = [km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2_r2.uuid]
            let qExpertUuids = [km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2_eNikola.uuid]
            let edited_1_event = e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2 {answerUuids = ChangedValue qAnswerUuids} :: EditOptionsQuestionEvent
            let edited_2_event = edited_1_event {referenceUuids = ChangedValue qReferenceUuids} :: EditOptionsQuestionEvent
            let edited_3_event = edited_2_event {expertUuids = ChangedValue qExpertUuids} :: EditOptionsQuestionEvent
            let reqState =
                  createTestMigratorStateWithEvents
                    []
                    [EditQuestionEvent' (EditOptionsQuestionEvent' edited_3_event)]
                    (Just km1)
            -- When:
            (Right resState) <- runInContext (migrate reqState) appContext
            -- Then:
            let (ConflictState (CorrectorConflict (Just (EditQuestionEvent' (EditOptionsQuestionEvent' resEvent))))) =
                  resState.migrationState
            resEvent.uuid `shouldNotBe` e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2.uuid
            resEvent.answerUuids
              `shouldBe` ChangedValue [q2_aYes_fuq1_aYes_fuq2_answerYes.uuid, q2_aYes_fuq1_aYes_fuq2_answerNo.uuid]
            resEvent.referenceUuids `shouldBe` ChangedValue []
            resEvent.expertUuids `shouldBe` ChangedValue []
        it "QType - QuestionTypeOptions: Event - all KM uuids exists, new added in event but without existing in KM" $
          -- Given:
          do
            let qAnswerUuids =
                  [q2_aYes_fuq1_aYes_fuq2_answerYes.uuid]
                    ++ [fromJust . U.fromString $ "54992efb-4738-4f00-9c69-979d28cee5ff"]
                    ++ [q2_aYes_fuq1_aYes_fuq2_answerNo.uuid]
            let qReferenceUuids =
                  [km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2_r2.uuid]
                    ++ [fromJust . U.fromString $ "bdbd95fd-8ea5-485d-9486-ef452b0a661e"]
                    ++ [km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2_r1.uuid]
            let qExpertUuids =
                  [km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2_eNikola.uuid]
                    ++ [fromJust . U.fromString $ "e47df67f-7e6d-4e0f-950d-5035a48087a0"]
                    ++ [km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2_eAlbert.uuid]
            let edited_1_event = e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2 {answerUuids = ChangedValue qAnswerUuids} :: EditOptionsQuestionEvent
            let edited_2_event = edited_1_event {referenceUuids = ChangedValue qReferenceUuids} :: EditOptionsQuestionEvent
            let edited_3_event = edited_2_event {expertUuids = ChangedValue qExpertUuids} :: EditOptionsQuestionEvent
            let reqState =
                  createTestMigratorStateWithEvents
                    []
                    [EditQuestionEvent' (EditOptionsQuestionEvent' edited_3_event)]
                    (Just km1)
            -- When:
            (Right resState) <- runInContext (migrate reqState) appContext
            -- Then:
            let (ConflictState (CorrectorConflict (Just (EditQuestionEvent' (EditOptionsQuestionEvent' resEvent))))) =
                  resState.migrationState
            resEvent.uuid `shouldNotBe` e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2.uuid
            resEvent.answerUuids
              `shouldBe` ChangedValue [q2_aYes_fuq1_aYes_fuq2_answerYes.uuid, q2_aYes_fuq1_aYes_fuq2_answerNo.uuid]
            resEvent.referenceUuids `shouldBe` ChangedValue []
            resEvent.expertUuids `shouldBe` ChangedValue []
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
            (Right resState) <- runInContext (migrate reqState) appContext
            -- Then:
            let (ConflictState (CorrectorConflict (Just (EditQuestionEvent' (EditListQuestionEvent' resEvent))))) =
                  resState.migrationState
            resEvent.uuid `shouldBe` e_km1_ch2_ansMaybe6_fuq4.uuid
            resEvent.itemTemplateQuestionUuids `shouldBe` e_km1_ch2_ansMaybe6_fuq4.itemTemplateQuestionUuids
        it "QType - QuestionTypeList: Event - some KM uuids missing, no new added in event" $
          -- Given:
          do
            let itQuestionUuids = [q4_it1_q6_aYes_fuq4_it_question2.uuid]
            let edited_e_km1_ch2_ansMaybe6_fuq4 =
                  e_km1_ch2_ansMaybe6_fuq4 {itemTemplateQuestionUuids = ChangedValue itQuestionUuids} :: EditListQuestionEvent
            let reqState =
                  createTestMigratorStateWithEvents
                    []
                    [EditQuestionEvent' (EditListQuestionEvent' edited_e_km1_ch2_ansMaybe6_fuq4)]
                    (Just km1WithQ4)
            -- When:
            (Right resState) <- runInContext (migrate reqState) appContext
            -- Then:
            let (ConflictState (CorrectorConflict (Just (EditQuestionEvent' (EditListQuestionEvent' resEvent))))) =
                  resState.migrationState
            resEvent.uuid `shouldNotBe` e_km1_ch2_ansMaybe6_fuq4.uuid
            let (ChangedValue resItqus) = resEvent.itemTemplateQuestionUuids
            resItqus `shouldBe` [q4_it1_q6_aYes_fuq4_it_question2.uuid, q4_it1_q6_aYes_fuq4_it_question1.uuid]
        it "QType - QuestionTypeList: Event - all KM uuids exists, new added in event but without existing in KM" $
          -- Given:
          do
            let itQuestionUuids =
                  [q4_it1_q6_aYes_fuq4_it_question2.uuid]
                    ++ [fromJust . U.fromString $ "54992efb-4738-4f00-9c69-979d28cee5ff"]
                    ++ [q4_it1_q6_aYes_fuq4_it_question1.uuid]
            let edited_e_km1_ch2_ansMaybe6_fuq4 =
                  e_km1_ch2_ansMaybe6_fuq4 {itemTemplateQuestionUuids = ChangedValue itQuestionUuids} :: EditListQuestionEvent
            let reqState =
                  createTestMigratorStateWithEvents
                    []
                    [EditQuestionEvent' (EditListQuestionEvent' edited_e_km1_ch2_ansMaybe6_fuq4)]
                    (Just km1WithQ4)
            -- When:
            (Right resState) <- runInContext (migrate reqState) appContext
            -- Then:
            let (ConflictState (CorrectorConflict (Just (EditQuestionEvent' (EditListQuestionEvent' resEvent))))) =
                  resState.migrationState
            resEvent.uuid `shouldNotBe` e_km1_ch2_ansMaybe6_fuq4.uuid
            let (ChangedValue resItqus) = resEvent.itemTemplateQuestionUuids
            resItqus `shouldBe` [q4_it1_q6_aYes_fuq4_it_question2.uuid, q4_it1_q6_aYes_fuq4_it_question1.uuid]
      -- -------------------------------------------------------------
      -- -------------------------------------------------------------
      describe "Sanatize: EditQuestionEvent (AnswerItemTemplate)" $ do
        it "QType - QuestionTypeOptions: Event - all KM uuids exists, no new added in event" $
          -- Given:
          do
            let reqState = createTestMigratorStateWithEvents [] [EditQuestionEvent' e_km1_ch2_q4_it1_q6'] (Just km1WithQ4)
            -- When:
            (Right resState) <- runInContext (migrate reqState) appContext
            -- Then:
            let (ConflictState (CorrectorConflict (Just (EditQuestionEvent' (EditOptionsQuestionEvent' resEvent))))) =
                  resState.migrationState
            resEvent.uuid `shouldBe` e_km1_ch2_q4_it1_q6.uuid
            resEvent.answerUuids `shouldBe` e_km1_ch2_q4_it1_q6.answerUuids
            resEvent.referenceUuids `shouldBe` e_km1_ch2_q4_it1_q6.referenceUuids
            resEvent.expertUuids `shouldBe` e_km1_ch2_q4_it1_q6.expertUuids
        it "QType - QuestionTypeOptions: Event - some KM uuids missing, no new added in event" $
          -- Given:
          do
            let qAnswerUuids = [q4_it1_q6_answerYes.uuid]
            let qReferenceUuids = [km1_ch2_q6_r2.uuid]
            let qExpertUuids = [km1_ch2_q6_eNikola.uuid]
            let edited_1_event = e_km1_ch2_q4_it1_q6 {answerUuids = ChangedValue qAnswerUuids} :: EditOptionsQuestionEvent
            let edited_2_event = edited_1_event {referenceUuids = ChangedValue qReferenceUuids} :: EditOptionsQuestionEvent
            let edited_3_event = edited_2_event {expertUuids = ChangedValue qExpertUuids} :: EditOptionsQuestionEvent
            let reqState =
                  createTestMigratorStateWithEvents
                    []
                    [EditQuestionEvent' (EditOptionsQuestionEvent' edited_3_event)]
                    (Just km1WithQ4)
            -- When:
            (Right resState) <- runInContext (migrate reqState) appContext
            -- Then:
            let (ConflictState (CorrectorConflict (Just (EditQuestionEvent' (EditOptionsQuestionEvent' resEvent))))) =
                  resState.migrationState
            resEvent.uuid `shouldNotBe` e_km1_ch2_q4_it1_q6.uuid
            resEvent.answerUuids `shouldBe` ChangedValue [q4_it1_q6_answerYes.uuid, q4_it1_q6_answerNo.uuid]
            resEvent.referenceUuids `shouldBe` ChangedValue [km1_ch2_q6_r2.uuid, km1_ch2_q6_r1.uuid]
            resEvent.expertUuids `shouldBe` ChangedValue [km1_ch2_q6_eNikola.uuid, km1_ch2_q6_eAlbert.uuid]
        it "QType - QuestionTypeOptions: Event - all KM uuids exists, new added in event but without existing in KM" $
          -- Given:
          do
            let qAnswerUuids =
                  [q4_it1_q6_answerYes.uuid]
                    ++ [fromJust . U.fromString $ "54992efb-4738-4f00-9c69-979d28cee5ff"]
                    ++ [q2_aYes_fuq1_aYes_fuq2_answerNo.uuid]
            let qReferenceUuids =
                  [km1_ch2_q6_r2.uuid]
                    ++ [fromJust . U.fromString $ "bdbd95fd-8ea5-485d-9486-ef452b0a661e"]
                    ++ [km1_ch2_q6_r1.uuid]
            let qExpertUuids =
                  [km1_ch2_q6_eNikola.uuid]
                    ++ [fromJust . U.fromString $ "e47df67f-7e6d-4e0f-950d-5035a48087a0"]
                    ++ [km1_ch2_q6_eAlbert.uuid]
            let edited_1_event = e_km1_ch2_q4_it1_q6 {answerUuids = ChangedValue qAnswerUuids} :: EditOptionsQuestionEvent
            let edited_2_event = edited_1_event {referenceUuids = ChangedValue qReferenceUuids} :: EditOptionsQuestionEvent
            let edited_3_event = edited_2_event {expertUuids = ChangedValue qExpertUuids} :: EditOptionsQuestionEvent
            let reqState =
                  createTestMigratorStateWithEvents
                    []
                    [EditQuestionEvent' (EditOptionsQuestionEvent' edited_3_event)]
                    (Just km1WithQ4)
            -- When:
            (Right resState) <- runInContext (migrate reqState) appContext
            -- Then:
            let (ConflictState (CorrectorConflict (Just (EditQuestionEvent' (EditOptionsQuestionEvent' resEvent))))) =
                  resState.migrationState
            resEvent.uuid `shouldNotBe` e_km1_ch2_q4_it1_q6.uuid
            resEvent.answerUuids `shouldBe` ChangedValue [q4_it1_q6_answerYes.uuid, q4_it1_q6_answerNo.uuid]
            resEvent.referenceUuids `shouldBe` ChangedValue [km1_ch2_q6_r2.uuid, km1_ch2_q6_r1.uuid]
            resEvent.expertUuids `shouldBe` ChangedValue [km1_ch2_q6_eNikola.uuid, km1_ch2_q6_eAlbert.uuid]
        -- -------------------------------------------------------------
        it "QType - QuestionTypeList: Event - all KM uuids exists, no new added in event" $
          -- Given:
          do
            let reqState = createTestMigratorStateWithEvents [] [EditQuestionEvent' e_km1_ch2_q4_it1_q5'] (Just km1WithQ4)
            -- When:
            (Right resState) <- runInContext (migrate reqState) appContext
            -- Then:
            let (ConflictState (CorrectorConflict (Just (EditQuestionEvent' (EditListQuestionEvent' resEvent))))) =
                  resState.migrationState
            resEvent.uuid `shouldBe` e_km1_ch2_q4_it1_q5.uuid
            resEvent.itemTemplateQuestionUuids `shouldBe` e_km1_ch2_q4_it1_q5.itemTemplateQuestionUuids
        it "QType - QuestionTypeList: Event - some KM uuids missing, no new added in event" $
          -- Given:
          do
            let itQuestionUuids = [q4_it1_q5_it2_question8.uuid]
            let edited_e_km1_ch2_q4_it1_q5 = e_km1_ch2_q4_it1_q5 {itemTemplateQuestionUuids = ChangedValue itQuestionUuids} :: EditListQuestionEvent
            let reqState =
                  createTestMigratorStateWithEvents
                    []
                    [EditQuestionEvent' (EditListQuestionEvent' edited_e_km1_ch2_q4_it1_q5)]
                    (Just km1WithQ4)
            -- When:
            (Right resState) <- runInContext (migrate reqState) appContext
            -- Then:
            let (ConflictState (CorrectorConflict (Just (EditQuestionEvent' (EditListQuestionEvent' resEvent))))) =
                  resState.migrationState
            resEvent.uuid `shouldNotBe` e_km1_ch2_q4_it1_q5.uuid
            let (ChangedValue resItqus) = resEvent.itemTemplateQuestionUuids
            resItqus `shouldBe` [q4_it1_q5_it2_question8.uuid, q4_it1_q5_it2_question7.uuid]
        it "QType - QuestionTypeList: Event - all KM uuids exists, new added in event but without existing in KM" $
          -- Given:
          do
            let itQuestionUuids =
                  [q4_it1_q5_it2_question8.uuid]
                    ++ [fromJust . U.fromString $ "54992efb-4738-4f00-9c69-979d28cee5ff"]
                    ++ [q4_it1_q5_it2_question7.uuid]
            let edited_e_km1_ch2_q4_it1_q5 = e_km1_ch2_q4_it1_q5 {itemTemplateQuestionUuids = ChangedValue itQuestionUuids} :: EditListQuestionEvent
            let reqState =
                  createTestMigratorStateWithEvents
                    []
                    [EditQuestionEvent' . EditListQuestionEvent' $ edited_e_km1_ch2_q4_it1_q5]
                    (Just km1WithQ4)
            -- When:
            (Right resState) <- runInContext (migrate reqState) appContext
            -- Then:
            let (ConflictState (CorrectorConflict (Just (EditQuestionEvent' (EditListQuestionEvent' resEvent))))) =
                  resState.migrationState
            resEvent.uuid `shouldNotBe` e_km1_ch2_q4_it1_q5.uuid
            let (ChangedValue resItqus) = resEvent.itemTemplateQuestionUuids
            resItqus `shouldBe` [q4_it1_q5_it2_question8.uuid, q4_it1_q5_it2_question7.uuid]
