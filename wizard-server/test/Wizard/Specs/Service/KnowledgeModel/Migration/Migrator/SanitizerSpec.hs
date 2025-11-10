module Wizard.Specs.Service.KnowledgeModel.Migration.Migrator.SanitizerSpec where

import Data.Maybe
import qualified Data.UUID as U
import Test.Hspec hiding (shouldBe, shouldNotBe)
import Test.Hspec.Expectations.Pretty

import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.AnswersAndFollowUpQuestions
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Chapters
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Event.KnowledgeModelEvents
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Experts
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Integrations
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Questions
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.References
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Tags
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Answer.AnswerEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Chapter.ChapterEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModel.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEventField
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Question.QuestionEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Wizard.Model.KnowledgeModel.Migration.KnowledgeModelMigration
import Wizard.Service.KnowledgeModel.Migration.Migrator.Migrator

import Wizard.Specs.Common
import Wizard.Specs.Service.KnowledgeModel.Migration.Migrator.Common

sanitizerSpec appContext =
  describe "Sanitizer" $
    -- -------------------------------------------------------------
    -- -------------------------------------------------------------
    do
      describe "Sanitize EditKnowledgeEvent" $ do
        it "Event - all KM uuids exists, no new added in event" $
          -- Given:
          do
            let reqState = createTestMigratorStateWithEvents [] [e_km1] (Just km1)
            -- When:
            (Right resState) <- runInContext (migrate reqState) appContext
            -- Then:
            let (ConflictKnowledgeModelMigrationState (Just resEvent)) = resState.state
            let (EditKnowledgeModelEvent' resEventContent) = resEvent.content
            resEvent.uuid `shouldBe` e_km1.uuid
            resEventContent.chapterUuids `shouldBe` e_km1__content.chapterUuids
            resEventContent.tagUuids `shouldBe` e_km1__content.tagUuids
            resEventContent.integrationUuids `shouldBe` e_km1__content.integrationUuids
        it "Event - some KM uuids missing, no new added in event" $
          -- Given:
          do
            let kmChapterUuids = fmap (.uuid) [chapter3, chapter2]
            let kmTagUuids = (.uuid) <$> [tagBioinformatics]
            let kmIntegrationUuids = [widgetPortal.uuid, bioPortal.uuid]
            let (EditKnowledgeModelEvent' eventContent) = e_km1.content
            let editedEvent =
                  e_km1
                    { content =
                        EditKnowledgeModelEvent' $
                          eventContent
                            { chapterUuids = ChangedValue kmChapterUuids
                            , tagUuids = ChangedValue kmTagUuids
                            , integrationUuids = ChangedValue kmIntegrationUuids
                            }
                    }
                  :: KnowledgeModelEvent
            let reqState = createTestMigratorStateWithEvents [] [editedEvent] (Just km1)
            -- When:
            (Right resState) <- runInContext (migrate reqState) appContext
            -- Then:
            let (ConflictKnowledgeModelMigrationState (Just resEvent)) = resState.state
            let (EditKnowledgeModelEvent' resEventContent) = resEvent.content
            resEvent.uuid `shouldNotBe` e_km1.uuid
            resEventContent.chapterUuids `shouldBe` ChangedValue [chapter3.uuid, chapter2.uuid, chapter1.uuid]
            resEventContent.tagUuids `shouldBe` ChangedValue [tagBioinformatics.uuid, tagDataScience.uuid]
            resEventContent.integrationUuids `shouldBe` ChangedValue [widgetPortal.uuid, bioPortal.uuid, ontologyPortal.uuid]
        it "Event - all KM uuids exists, new added in event but without existing in KM" $
          -- Given:
          do
            let kmChapterUuids =
                  [chapter3.uuid]
                    ++ [chapter2.uuid]
                    ++ [fromJust . U.fromString $ "54992efb-4738-4f00-9c69-979d28cee5ff"]
                    ++ [chapter1.uuid]
            let kmTagUuids =
                  [tagBioinformatics.uuid]
                    ++ [fromJust . U.fromString $ "b28d289b-e373-49a2-9c91-b153cb62d894"]
                    ++ [tagDataScience.uuid]
            let kmIntegrationUuids =
                  [bioPortal.uuid]
                    ++ [fromJust . U.fromString $ "eb75a1a7-2760-446a-9a44-17b8f38679bf"]
                    ++ [widgetPortal.uuid, ontologyPortal.uuid]
            let (EditKnowledgeModelEvent' eventContent) = e_km1.content
            let editedEvent =
                  e_km1
                    { content =
                        EditKnowledgeModelEvent' $
                          eventContent
                            { chapterUuids = ChangedValue kmChapterUuids
                            , tagUuids = ChangedValue kmTagUuids
                            , integrationUuids = ChangedValue kmIntegrationUuids
                            }
                    }
                  :: KnowledgeModelEvent
            let reqState = createTestMigratorStateWithEvents [] [editedEvent] (Just km1)
            -- When:
            (Right resState) <- runInContext (migrate reqState) appContext
            -- Then:
            let (ConflictKnowledgeModelMigrationState (Just resEvent)) = resState.state
            let (EditKnowledgeModelEvent' resEventContent) = resEvent.content
            resEvent.uuid `shouldNotBe` e_km1.uuid
            resEventContent.chapterUuids `shouldBe` ChangedValue [chapter3.uuid, chapter2.uuid, chapter1.uuid]
            resEventContent.tagUuids `shouldBe` ChangedValue [tagBioinformatics.uuid, tagDataScience.uuid]
            resEventContent.integrationUuids `shouldBe` ChangedValue [bioPortal.uuid, widgetPortal.uuid, ontologyPortal.uuid]
      -- -------------------------------------------------------------
      -- -------------------------------------------------------------
      describe "Sanitize: EditChapterEvent" $ do
        it "Event - all KM uuids exists, no new added in event" $
          -- Given:
          do
            let reqState = createTestMigratorStateWithEvents [] [e_km1_ch1] (Just km1)
            -- When:
            (Right resState) <- runInContext (migrate reqState) appContext
            -- Then:
            let (ConflictKnowledgeModelMigrationState (Just resEvent)) = resState.state
            let (EditChapterEvent' resEventContent) = resEvent.content
            resEvent.uuid `shouldBe` e_km1_ch1.uuid
            resEventContent.questionUuids `shouldBe` e_km1_ch1__content.questionUuids
        it "Event - some KM uuids missing, no new added in event" $
          -- Given:
          do
            let chQuestionUuids = [question2.uuid]
            let (EditChapterEvent' eventContent) = e_km1_ch1.content
            let editedEvent =
                  e_km1_ch1
                    { content =
                        EditChapterEvent' $
                          eventContent
                            { questionUuids = ChangedValue chQuestionUuids
                            }
                    }
                  :: KnowledgeModelEvent
            let reqState = createTestMigratorStateWithEvents [] [editedEvent] (Just km1)
            -- When:
            (Right resState) <- runInContext (migrate reqState) appContext
            -- Then:
            let (ConflictKnowledgeModelMigrationState (Just resEvent)) = resState.state
            let (EditChapterEvent' resEventContent) = resEvent.content
            resEvent.uuid `shouldNotBe` e_km1_ch1.uuid
            resEventContent.questionUuids `shouldBe` ChangedValue [question2.uuid, question1.uuid]
        it "Event - all KM uuids exists, new added in event but without existing in KM" $
          -- Given:
          do
            let chQuestionUuids =
                  [question2.uuid]
                    ++ [fromJust . U.fromString $ "54992efb-4738-4f00-9c69-979d28cee5ff"]
                    ++ [question1.uuid]
            let (EditChapterEvent' eventContent) = e_km1_ch1.content
            let editedEvent =
                  e_km1_ch1
                    { content =
                        EditChapterEvent' $
                          eventContent
                            { questionUuids = ChangedValue chQuestionUuids
                            }
                    }
                  :: KnowledgeModelEvent
            let reqState = createTestMigratorStateWithEvents [] [editedEvent] (Just km1)
            -- When:
            (Right resState) <- runInContext (migrate reqState) appContext
            -- Then:
            let (ConflictKnowledgeModelMigrationState (Just resEvent)) = resState.state
            let (EditChapterEvent' resEventContent) = resEvent.content
            resEvent.uuid `shouldNotBe` e_km1_ch1.uuid
            resEventContent.questionUuids `shouldBe` ChangedValue [question2.uuid, question1.uuid]
      -- -------------------------------------------------------------
      -- -------------------------------------------------------------
      describe "Sanitize: EditQuestionEvent" $ do
        it "QType - QuestionTypeOptions: Event - all KM uuids exists, no new added in event" $
          -- Given:
          do
            let reqState = createTestMigratorStateWithEvents [] [e_km1_ch1_q2] (Just km1)
            -- When:
            (Right resState) <- runInContext (migrate reqState) appContext
            -- Then:
            let (ConflictKnowledgeModelMigrationState (Just resEvent)) = resState.state
            let (EditQuestionEvent' (EditOptionsQuestionEvent' resEventContent)) = resEvent.content
            resEvent.uuid `shouldBe` e_km1_ch1_q2.uuid
            resEventContent.answerUuids `shouldBe` e_km1_ch1_q2__content.answerUuids
            resEventContent.referenceUuids `shouldBe` e_km1_ch1_q2__content.referenceUuids
            resEventContent.expertUuids `shouldBe` e_km1_ch1_q2__content.expertUuids
        it "QType - QuestionTypeOptions: Event - some KM uuids missing, no new added in event" $
          -- Given:
          do
            let qAnswerUuids = [q2_answerYes.uuid]
            let qReferenceUuids = [km1_ch1_q2_r2.uuid]
            let qExpertUuids = [km1_ch1_q2_eNikola.uuid]
            let (EditQuestionEvent' (EditOptionsQuestionEvent' eventContent)) = e_km1_ch1_q2.content
            let editedEvent =
                  e_km1_ch1_q2
                    { content =
                        EditQuestionEvent' $
                          EditOptionsQuestionEvent' $
                            eventContent
                              { answerUuids = ChangedValue qAnswerUuids
                              , referenceUuids = ChangedValue qReferenceUuids
                              , expertUuids = ChangedValue qExpertUuids
                              }
                    }
                  :: KnowledgeModelEvent
            let reqState = createTestMigratorStateWithEvents [] [editedEvent] (Just km1)
            -- When:
            (Right resState) <- runInContext (migrate reqState) appContext
            -- Then:
            let (ConflictKnowledgeModelMigrationState (Just resEvent)) = resState.state
            let (EditQuestionEvent' (EditOptionsQuestionEvent' resEventContent)) = resEvent.content
            resEvent.uuid `shouldNotBe` e_km1_ch1_q2.uuid
            resEventContent.answerUuids `shouldBe` ChangedValue [q2_answerYes.uuid, q2_answerNo.uuid]
            resEventContent.referenceUuids `shouldBe` ChangedValue [km1_ch1_q2_r2.uuid, km1_ch1_q2_r1.uuid]
            resEventContent.expertUuids `shouldBe` ChangedValue [km1_ch1_q2_eNikola.uuid, km1_ch1_q2_eAlbert.uuid]
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
            let (EditQuestionEvent' (EditOptionsQuestionEvent' eventContent)) = e_km1_ch1_q2.content
            let editedEvent =
                  e_km1_ch1_q2
                    { content =
                        EditQuestionEvent' $
                          EditOptionsQuestionEvent' $
                            eventContent
                              { answerUuids = ChangedValue qAnswerUuids
                              , referenceUuids = ChangedValue qReferenceUuids
                              , expertUuids = ChangedValue qExpertUuids
                              }
                    }
                  :: KnowledgeModelEvent
            let reqState = createTestMigratorStateWithEvents [] [editedEvent] (Just km1)
            -- When:
            (Right resState) <- runInContext (migrate reqState) appContext
            -- Then:
            let (ConflictKnowledgeModelMigrationState (Just resEvent)) = resState.state
            let (EditQuestionEvent' (EditOptionsQuestionEvent' resEventContent)) = resEvent.content
            resEvent.uuid `shouldNotBe` e_km1_ch1_q2.uuid
            resEventContent.answerUuids `shouldBe` ChangedValue [q2_answerYes.uuid, q2_answerNo.uuid]
            resEventContent.referenceUuids `shouldBe` ChangedValue [km1_ch1_q2_r2.uuid, km1_ch1_q2_r1.uuid]
            resEventContent.expertUuids `shouldBe` ChangedValue [km1_ch1_q2_eNikola.uuid, km1_ch1_q2_eAlbert.uuid]
        -- -------------------------------------------------------------
        it "QType - QuestionTypeList: Event - all KM uuids exists, no new added in event" $
          -- Given:
          do
            let reqState = createTestMigratorStateWithEvents [] [e_km1_ch2_q4] (Just km1WithQ4)
            -- When:
            (Right resState) <- runInContext (migrate reqState) appContext
            -- Then:
            let (ConflictKnowledgeModelMigrationState (Just resEvent)) = resState.state
            let (EditQuestionEvent' (EditListQuestionEvent' resEventContent)) = resEvent.content
            resEvent.uuid `shouldNotBe` e_km1_ch1_q2.uuid
            let (ChangedValue resUuids) = resEventContent.itemTemplateQuestionUuids
            resUuids `shouldBe` [q4_it1_question6.uuid, q4_it1_question5.uuid]
        it "QType - QuestionTypeList: Event - some KM uuids missing, no new added in event" $
          -- Given:
          do
            let itQuestionUuids = [q4_it1_question6.uuid]
            let (EditQuestionEvent' (EditListQuestionEvent' eventContent)) = e_km1_ch2_q4.content
            let editedEvent =
                  e_km1_ch2_q4
                    { content =
                        EditQuestionEvent' $
                          EditListQuestionEvent' $
                            eventContent
                              { itemTemplateQuestionUuids = ChangedValue itQuestionUuids
                              }
                    }
                  :: KnowledgeModelEvent
            let reqState = createTestMigratorStateWithEvents [] [editedEvent] (Just km1WithQ4)
            -- When:
            (Right resState) <- runInContext (migrate reqState) appContext
            -- Then:
            let (ConflictKnowledgeModelMigrationState (Just resEvent)) = resState.state
            let (EditQuestionEvent' (EditListQuestionEvent' resEventContent)) = resEvent.content
            resEvent.uuid `shouldNotBe` e_km1_ch2_q4.uuid
            let (ChangedValue resUuids) = resEventContent.itemTemplateQuestionUuids
            resUuids `shouldBe` [q4_it1_question6.uuid, q4_it1_question5.uuid]
        it "QType - QuestionTypeList: Event - all KM uuids exists, new added in event but without existing in KM" $
          -- Given:
          do
            let itQuestionUuids =
                  [q4_it1_question6.uuid]
                    ++ [fromJust . U.fromString $ "54992efb-4738-4f00-9c69-979d28cee5ff"]
                    ++ [q4_it1_question5.uuid]
            let (EditQuestionEvent' (EditListQuestionEvent' eventContent)) = e_km1_ch2_q4.content
            let editedEvent =
                  e_km1_ch2_q4
                    { content =
                        EditQuestionEvent' $
                          EditListQuestionEvent' $
                            eventContent
                              { itemTemplateQuestionUuids = ChangedValue itQuestionUuids
                              }
                    }
                  :: KnowledgeModelEvent
            let reqState = createTestMigratorStateWithEvents [] [editedEvent] (Just km1WithQ4)
            -- When:
            (Right resState) <- runInContext (migrate reqState) appContext
            -- Then:
            let (ConflictKnowledgeModelMigrationState (Just resEvent)) = resState.state
            let (EditQuestionEvent' (EditListQuestionEvent' resEventContent)) = resEvent.content
            resEvent.uuid `shouldNotBe` e_km1_ch2_q4.uuid
            let (ChangedValue resUuids) = resEventContent.itemTemplateQuestionUuids
            resUuids `shouldBe` [q4_it1_question6.uuid, q4_it1_question5.uuid]
      -- -------------------------------------------------------------
      -- -------------------------------------------------------------
      describe "Sanitize: EditAnswerEvent" $ do
        it "Event - all KM uuids exists, no new added in event" $
          -- Given:
          do
            let reqState = createTestMigratorStateWithEvents [] [e_km1_ch1_q2_aYes1_2] (Just km1)
            -- When:
            (Right resState) <- runInContext (migrate reqState) appContext
            -- Then:
            let (ConflictKnowledgeModelMigrationState (Just resEvent)) = resState.state
            let (EditAnswerEvent' resEventContent) = resEvent.content
            resEvent.uuid `shouldBe` e_km1_ch1_q2_aYes1_2.uuid
            resEventContent.followUpUuids `shouldBe` e_km1_ch1_q2_aYes1_2__content.followUpUuids
        it "Event - some KM uuids missing, no new added in event" $
          -- Given:
          do
            let ansFollowUpUuids = []
            let (EditAnswerEvent' eventContent) = e_km1_ch1_q2_aYes1_2.content
            let editedEvent =
                  e_km1_ch1_q2_aYes1_2
                    { content =
                        EditAnswerEvent' $
                          eventContent
                            { followUpUuids = ChangedValue ansFollowUpUuids
                            }
                    }
                  :: KnowledgeModelEvent
            let reqState = createTestMigratorStateWithEvents [] [editedEvent] (Just km1)
            -- When:
            (Right resState) <- runInContext (migrate reqState) appContext
            -- Then:
            let (ConflictKnowledgeModelMigrationState (Just resEvent)) = resState.state
            let (EditAnswerEvent' resEventContent) = resEvent.content
            resEvent.uuid `shouldNotBe` e_km1_ch1_q2_aYes1_2.uuid
            resEventContent.followUpUuids `shouldBe` ChangedValue [q2_aYes_fuQuestion1.uuid]
        it "Event - all KM uuids exists, new added in event but without existing in KM" $
          -- Given:
          do
            let ansFollowUpUuids =
                  [fromJust . U.fromString $ "54992efb-4738-4f00-9c69-979d28cee5ff"]
                    ++ [q2_aYes_fuQuestion1.uuid]
                    ++ [fromJust . U.fromString $ "3534d39b-e493-4e9f-b84d-f302c4077b5c"]
            let (EditAnswerEvent' eventContent) = e_km1_ch1_q2_aYes1_2.content
            let editedEvent =
                  e_km1_ch1_q2_aYes1_2
                    { content =
                        EditAnswerEvent' $
                          eventContent
                            { followUpUuids = ChangedValue ansFollowUpUuids
                            }
                    }
                  :: KnowledgeModelEvent
            let reqState = createTestMigratorStateWithEvents [] [editedEvent] (Just km1)
            -- When:
            (Right resState) <- runInContext (migrate reqState) appContext
            -- Then:
            let (ConflictKnowledgeModelMigrationState (Just resEvent)) = resState.state
            let (EditAnswerEvent' resEventContent) = resEvent.content
            resEvent.uuid `shouldNotBe` e_km1_ch1_q2_aYes1_2.uuid
            resEventContent.followUpUuids `shouldBe` ChangedValue [q2_aYes_fuQuestion1.uuid]
      -- -------------------------------------------------------------
      -- -------------------------------------------------------------
      describe "Sanitize: EditQuestionEvent (FollowUps)" $ do
        it "QType - QuestionTypeOptions: Event - all KM uuids exists, no new added in event" $
          -- Given:
          do
            let reqState = createTestMigratorStateWithEvents [] [e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2] (Just km1)
            -- When:
            (Right resState) <- runInContext (migrate reqState) appContext
            -- Then:
            let (ConflictKnowledgeModelMigrationState (Just resEvent)) = resState.state
            let (EditQuestionEvent' (EditOptionsQuestionEvent' resEventContent)) = resEvent.content
            resEvent.uuid `shouldBe` e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2.uuid
            resEventContent.answerUuids `shouldBe` e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2__content.answerUuids
            resEventContent.referenceUuids `shouldBe` e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2__content.referenceUuids
            resEventContent.expertUuids `shouldBe` e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2__content.expertUuids
        it "QType - QuestionTypeOptions: Event - some KM uuids missing, no new added in event" $
          -- Given:
          do
            let qAnswerUuids = [q2_aYes_fuq1_aYes_fuq2_answerYes.uuid]
            let qReferenceUuids = [km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2_r2.uuid]
            let qExpertUuids = [km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2_eNikola.uuid]
            let (EditQuestionEvent' (EditOptionsQuestionEvent' eventContent)) = e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2.content
            let editedEvent =
                  e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2
                    { content =
                        EditQuestionEvent' $
                          EditOptionsQuestionEvent' $
                            eventContent
                              { answerUuids = ChangedValue qAnswerUuids
                              , referenceUuids = ChangedValue qReferenceUuids
                              , expertUuids = ChangedValue qExpertUuids
                              }
                    }
                  :: KnowledgeModelEvent
            let reqState = createTestMigratorStateWithEvents [] [editedEvent] (Just km1)
            -- When:
            (Right resState) <- runInContext (migrate reqState) appContext
            -- Then:
            let (ConflictKnowledgeModelMigrationState (Just resEvent)) = resState.state
            let (EditQuestionEvent' (EditOptionsQuestionEvent' resEventContent)) = resEvent.content
            resEvent.uuid `shouldNotBe` e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2.uuid
            resEventContent.answerUuids `shouldBe` ChangedValue [q2_aYes_fuq1_aYes_fuq2_answerYes.uuid, q2_aYes_fuq1_aYes_fuq2_answerNo.uuid]
            resEventContent.referenceUuids `shouldBe` ChangedValue []
            resEventContent.expertUuids `shouldBe` ChangedValue []
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
            let (EditQuestionEvent' (EditOptionsQuestionEvent' eventContent)) = e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2.content
            let editedEvent =
                  e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2
                    { content =
                        EditQuestionEvent' $
                          EditOptionsQuestionEvent' $
                            eventContent
                              { answerUuids = ChangedValue qAnswerUuids
                              , referenceUuids = ChangedValue qReferenceUuids
                              , expertUuids = ChangedValue qExpertUuids
                              }
                    }
                  :: KnowledgeModelEvent
            let reqState = createTestMigratorStateWithEvents [] [editedEvent] (Just km1)
            -- When:
            (Right resState) <- runInContext (migrate reqState) appContext
            -- Then:
            let (ConflictKnowledgeModelMigrationState (Just resEvent)) = resState.state
            let (EditQuestionEvent' (EditOptionsQuestionEvent' resEventContent)) = resEvent.content
            resEvent.uuid `shouldNotBe` e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2.uuid
            resEventContent.answerUuids `shouldBe` ChangedValue [q2_aYes_fuq1_aYes_fuq2_answerYes.uuid, q2_aYes_fuq1_aYes_fuq2_answerNo.uuid]
            resEventContent.referenceUuids `shouldBe` ChangedValue []
            resEventContent.expertUuids `shouldBe` ChangedValue []
        -- -------------------------------------------------------------
        it "QType - QuestionTypeList: Event - all KM uuids exists, no new added in event" $
          -- Given:
          do
            let reqState = createTestMigratorStateWithEvents [] [e_km1_ch2_ansMaybe6_fuq4] (Just km1WithQ4)
            -- When:
            (Right resState) <- runInContext (migrate reqState) appContext
            -- Then:
            let (ConflictKnowledgeModelMigrationState (Just resEvent)) = resState.state
            let (EditQuestionEvent' (EditListQuestionEvent' resEventContent)) = resEvent.content
            resEvent.uuid `shouldBe` e_km1_ch2_ansMaybe6_fuq4.uuid
            resEventContent.itemTemplateQuestionUuids `shouldBe` e_km1_ch2_ansMaybe6_fuq4__content.itemTemplateQuestionUuids
        it "QType - QuestionTypeList: Event - some KM uuids missing, no new added in event" $
          -- Given:
          do
            let itQuestionUuids = [q4_it1_q6_aYes_fuq4_it_question2.uuid]
            let (EditQuestionEvent' (EditListQuestionEvent' eventContent)) = e_km1_ch2_ansMaybe6_fuq4.content
            let editedEvent =
                  e_km1_ch2_ansMaybe6_fuq4
                    { content =
                        EditQuestionEvent' $
                          EditListQuestionEvent' $
                            eventContent
                              { itemTemplateQuestionUuids = ChangedValue itQuestionUuids
                              }
                    }
                  :: KnowledgeModelEvent
            let reqState = createTestMigratorStateWithEvents [] [editedEvent] (Just km1WithQ4)
            -- When:
            (Right resState) <- runInContext (migrate reqState) appContext
            -- Then:
            let (ConflictKnowledgeModelMigrationState (Just resEvent)) = resState.state
            let (EditQuestionEvent' (EditListQuestionEvent' resEventContent)) = resEvent.content
            resEvent.uuid `shouldNotBe` e_km1_ch2_ansMaybe6_fuq4.uuid
            let (ChangedValue resUuids) = resEventContent.itemTemplateQuestionUuids
            resUuids `shouldBe` [q4_it1_q6_aYes_fuq4_it_question2.uuid, q4_it1_q6_aYes_fuq4_it_question1.uuid]
        it "QType - QuestionTypeList: Event - all KM uuids exists, new added in event but without existing in KM" $
          -- Given:
          do
            let itQuestionUuids =
                  [q4_it1_q6_aYes_fuq4_it_question2.uuid]
                    ++ [fromJust . U.fromString $ "54992efb-4738-4f00-9c69-979d28cee5ff"]
                    ++ [q4_it1_q6_aYes_fuq4_it_question1.uuid]
            let (EditQuestionEvent' (EditListQuestionEvent' eventContent)) = e_km1_ch2_ansMaybe6_fuq4.content
            let editedEvent =
                  e_km1_ch2_ansMaybe6_fuq4
                    { content =
                        EditQuestionEvent' $
                          EditListQuestionEvent' $
                            eventContent
                              { itemTemplateQuestionUuids = ChangedValue itQuestionUuids
                              }
                    }
                  :: KnowledgeModelEvent
            let reqState = createTestMigratorStateWithEvents [] [editedEvent] (Just km1WithQ4)
            -- When:
            (Right resState) <- runInContext (migrate reqState) appContext
            -- Then:
            let (ConflictKnowledgeModelMigrationState (Just resEvent)) = resState.state
            let (EditQuestionEvent' (EditListQuestionEvent' resEventContent)) = resEvent.content
            resEvent.uuid `shouldNotBe` e_km1_ch2_ansMaybe6_fuq4.uuid
            let (ChangedValue resUuids) = resEventContent.itemTemplateQuestionUuids
            resUuids `shouldBe` [q4_it1_q6_aYes_fuq4_it_question2.uuid, q4_it1_q6_aYes_fuq4_it_question1.uuid]
      -- -------------------------------------------------------------
      -- -------------------------------------------------------------
      describe "Sanitize: EditQuestionEvent (AnswerItemTemplate)" $ do
        it "QType - QuestionTypeOptions: Event - all KM uuids exists, no new added in event" $
          -- Given:
          do
            let reqState = createTestMigratorStateWithEvents [] [e_km1_ch2_q4_it1_q6] (Just km1WithQ4)
            -- When:
            (Right resState) <- runInContext (migrate reqState) appContext
            -- Then:
            let (ConflictKnowledgeModelMigrationState (Just resEvent)) = resState.state
            let (EditQuestionEvent' (EditOptionsQuestionEvent' resEventContent)) = resEvent.content
            resEvent.uuid `shouldBe` e_km1_ch2_q4_it1_q6.uuid
            resEventContent.answerUuids `shouldBe` e_km1_ch2_q4_it1_q6__content.answerUuids
            resEventContent.referenceUuids `shouldBe` e_km1_ch2_q4_it1_q6__content.referenceUuids
            resEventContent.expertUuids `shouldBe` e_km1_ch2_q4_it1_q6__content.expertUuids
        it "QType - QuestionTypeOptions: Event - some KM uuids missing, no new added in event" $
          -- Given:
          do
            let qAnswerUuids = [q4_it1_q6_answerYes.uuid]
            let qReferenceUuids = [km1_ch2_q6_r2.uuid]
            let qExpertUuids = [km1_ch2_q6_eNikola.uuid]
            let (EditQuestionEvent' (EditOptionsQuestionEvent' eventContent)) = e_km1_ch2_q4_it1_q6.content
            let editedEvent =
                  e_km1_ch2_q4_it1_q6
                    { content =
                        EditQuestionEvent' $
                          EditOptionsQuestionEvent' $
                            eventContent
                              { answerUuids = ChangedValue qAnswerUuids
                              , referenceUuids = ChangedValue qReferenceUuids
                              , expertUuids = ChangedValue qExpertUuids
                              }
                    }
                  :: KnowledgeModelEvent
            let reqState = createTestMigratorStateWithEvents [] [editedEvent] (Just km1WithQ4)
            -- When:
            (Right resState) <- runInContext (migrate reqState) appContext
            -- Then:
            let (ConflictKnowledgeModelMigrationState (Just resEvent)) = resState.state
            let (EditQuestionEvent' (EditOptionsQuestionEvent' resEventContent)) = resEvent.content
            resEvent.uuid `shouldNotBe` e_km1_ch2_q4_it1_q6.uuid
            resEventContent.answerUuids `shouldBe` ChangedValue [q4_it1_q6_answerYes.uuid, q4_it1_q6_answerNo.uuid]
            resEventContent.referenceUuids `shouldBe` ChangedValue [km1_ch2_q6_r2.uuid, km1_ch2_q6_r1.uuid]
            resEventContent.expertUuids `shouldBe` ChangedValue [km1_ch2_q6_eNikola.uuid, km1_ch2_q6_eAlbert.uuid]
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
            let (EditQuestionEvent' (EditOptionsQuestionEvent' eventContent)) = e_km1_ch2_q4_it1_q6.content
            let editedEvent =
                  e_km1_ch2_q4_it1_q6
                    { content =
                        EditQuestionEvent' $
                          EditOptionsQuestionEvent' $
                            eventContent
                              { answerUuids = ChangedValue qAnswerUuids
                              , referenceUuids = ChangedValue qReferenceUuids
                              , expertUuids = ChangedValue qExpertUuids
                              }
                    }
                  :: KnowledgeModelEvent
            let reqState = createTestMigratorStateWithEvents [] [editedEvent] (Just km1WithQ4)
            -- When:
            (Right resState) <- runInContext (migrate reqState) appContext
            -- Then:
            let (ConflictKnowledgeModelMigrationState (Just resEvent)) = resState.state
            let (EditQuestionEvent' (EditOptionsQuestionEvent' resEventContent)) = resEvent.content
            resEvent.uuid `shouldNotBe` e_km1_ch2_q4_it1_q6.uuid
            resEventContent.answerUuids `shouldBe` ChangedValue [q4_it1_q6_answerYes.uuid, q4_it1_q6_answerNo.uuid]
            resEventContent.referenceUuids `shouldBe` ChangedValue [km1_ch2_q6_r2.uuid, km1_ch2_q6_r1.uuid]
            resEventContent.expertUuids `shouldBe` ChangedValue [km1_ch2_q6_eNikola.uuid, km1_ch2_q6_eAlbert.uuid]
        -- -------------------------------------------------------------
        it "QType - QuestionTypeList: Event - all KM uuids exists, no new added in event" $
          -- Given:
          do
            let reqState = createTestMigratorStateWithEvents [] [e_km1_ch2_q4_it1_q5] (Just km1WithQ4)
            -- When:
            (Right resState) <- runInContext (migrate reqState) appContext
            -- Then:
            let (ConflictKnowledgeModelMigrationState (Just resEvent)) = resState.state
            let (EditQuestionEvent' (EditListQuestionEvent' resEventContent)) = resEvent.content
            resEvent.uuid `shouldBe` e_km1_ch2_q4_it1_q5.uuid
            resEventContent.itemTemplateQuestionUuids `shouldBe` e_km1_ch2_q4_it1_q5__content.itemTemplateQuestionUuids
        it "QType - QuestionTypeList: Event - some KM uuids missing, no new added in event" $
          -- Given:
          do
            let itQuestionUuids = [q4_it1_q5_it2_question8.uuid]
            let (EditQuestionEvent' (EditListQuestionEvent' eventContent)) = e_km1_ch2_q4_it1_q5.content
            let editedEvent =
                  e_km1_ch2_q4_it1_q5
                    { content =
                        EditQuestionEvent' $
                          EditListQuestionEvent' $
                            eventContent
                              { itemTemplateQuestionUuids = ChangedValue itQuestionUuids
                              }
                    }
                  :: KnowledgeModelEvent
            let reqState = createTestMigratorStateWithEvents [] [editedEvent] (Just km1WithQ4)
            -- When:
            (Right resState) <- runInContext (migrate reqState) appContext
            -- Then:
            let (ConflictKnowledgeModelMigrationState (Just resEvent)) = resState.state
            let (EditQuestionEvent' (EditListQuestionEvent' resEventContent)) = resEvent.content
            resEvent.uuid `shouldNotBe` e_km1_ch2_q4_it1_q5.uuid
            let (ChangedValue resUuids) = resEventContent.itemTemplateQuestionUuids
            resUuids `shouldBe` [q4_it1_q5_it2_question8.uuid, q4_it1_q5_it2_question7.uuid]
        it "QType - QuestionTypeList: Event - all KM uuids exists, new added in event but without existing in KM" $
          -- Given:
          do
            let itQuestionUuids =
                  [q4_it1_q5_it2_question8.uuid]
                    ++ [fromJust . U.fromString $ "54992efb-4738-4f00-9c69-979d28cee5ff"]
                    ++ [q4_it1_q5_it2_question7.uuid]
            let (EditQuestionEvent' (EditListQuestionEvent' eventContent)) = e_km1_ch2_q4_it1_q5.content
            let editedEvent =
                  e_km1_ch2_q4_it1_q5
                    { content =
                        EditQuestionEvent' $
                          EditListQuestionEvent' $
                            eventContent
                              { itemTemplateQuestionUuids = ChangedValue itQuestionUuids
                              }
                    }
                  :: KnowledgeModelEvent
            let reqState = createTestMigratorStateWithEvents [] [editedEvent] (Just km1WithQ4)
            -- When:
            (Right resState) <- runInContext (migrate reqState) appContext
            -- Then:
            let (ConflictKnowledgeModelMigrationState (Just resEvent)) = resState.state
            let (EditQuestionEvent' (EditListQuestionEvent' resEventContent)) = resEvent.content
            resEvent.uuid `shouldNotBe` e_km1_ch2_q4_it1_q5.uuid
            let (ChangedValue resUuids) = resEventContent.itemTemplateQuestionUuids
            resUuids `shouldBe` [q4_it1_q5_it2_question8.uuid, q4_it1_q5_it2_question7.uuid]
