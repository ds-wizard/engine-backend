module Wizard.Specs.Service.KnowledgeModel.Compilator.CompilatorSpec where

import qualified Data.List as L
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Wizard.Service.KnowledgeModel.Compilator.Compilator
import WizardLib.KnowledgeModel.Database.Migration.Development.Event.Data.Events
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.AnswersAndFollowUpQuestions
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Chapters
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Choices
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Experts
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Integrations
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Questions
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.References
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Resources
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Tags
import WizardLib.KnowledgeModel.Model.Event.Event
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModelLenses

compilatorSpec =
  describe "Compilator" $ do
    describe "Apply: No events" $ it "Apply: No events" $ do
      let emptyEvents = []
      let (Right computed) = compile (Just km1) emptyEvents
      let expected = km1
      computed `shouldBe` expected
    -- ---------------
    describe "Apply: KM Events" $ do
      it "Apply: AddKnowledgeEvent" $ do
        let (Right computed) = compile Nothing [AddKnowledgeModelEvent' a_km1]
        let expected = km1WithoutChaptersAndTagsAndIntegrations
        computed `shouldBe` expected
      it "Apply: EditKnowledgeEvent" $ do
        let (Right computed) = compile (Just km1) [EditKnowledgeModelEvent' e_km1]
        let expected = km1Edited
        computed `shouldBe` expected
    -- ---------------
    describe "Apply: Chapter Events" $ do
      it "Apply: AddChapterEvent" $ do
        let (Right computed) = compile (Just km1) [AddChapterEvent' a_km1_ch4]
        let expected =
              km1
                { entities =
                    km1.entities
                      { chapters = toMap [chapter1, chapter2, chapter3, chapter4WithoutQuestions]
                      }
                , chapterUuids = [chapter1.uuid, chapter2.uuid, chapter3.uuid, chapter4WithoutQuestions.uuid]
                }
        computed `shouldBe` expected
      it "Apply: EditChapterEvent" $ do
        let (Right computed) = compile (Just km1) [EditChapterEvent' e_km1_ch1]
        let expected =
              km1
                { entities =
                    km1.entities
                      { chapters = toMap [chapter1Edited, chapter2, chapter3]
                      }
                , chapterUuids = [chapter1Edited.uuid, chapter2.uuid, chapter3.uuid]
                }
        computed `shouldBe` expected
      it "Apply: DeleteChapterEvent" $ do
        let (Right computed) = compile (Just km1) [DeleteChapterEvent' d_km1_ch1]
        let expected =
              km1
                { entities =
                    km1.entities
                      { chapters = toMap [chapter2, chapter3]
                      , questions = toMap [question3', question9', question10', question11', question12', question13', question14']
                      , answers = toMap [q3_answerNo, q3_answerYes]
                      , experts = toMap []
                      , references = toMap []
                      }
                , chapterUuids = [chapter2.uuid, chapter3.uuid]
                }
        computed `shouldBe` expected
    -- ---------------
    describe "Apply: Question Events" $ do
      it "Apply: AddQuestionEvent" $ do
        let (Right computed) = compile (Just km1) [AddQuestionEvent' a_km1_ch1_q3']
        let expected =
              putInChaptersM chapter1.uuid (chapter1 {questionUuids = [question1.uuid, question2.uuid, question3.uuid]}) $
                km1
                  { entities =
                      km1.entities
                        { questions =
                            toMap
                              [ question1'
                              , question2'
                              , q2_aYes_fuQuestion1'
                              , q2_aYes_fuq1_aYes_fuQuestion2'
                              , question3Plain'
                              , question9'
                              , question10'
                              , question11'
                              , question12'
                              , question13'
                              , question14'
                              ]
                        }
                  }
        computed `shouldBe` expected
      it "Apply: EditQuestionEvent" $ do
        let (Right computed) = compile (Just km1) [EditQuestionEvent' e_km1_ch1_q2']
        let expected =
              km1
                { entities =
                    km1.entities
                      { questions =
                          toMap
                            [ question1'
                            , question2Edited'
                            , q2_aYes_fuQuestion1'
                            , q2_aYes_fuq1_aYes_fuQuestion2'
                            , question3'
                            , question9'
                            , question10'
                            , question11'
                            , question12'
                            , question13'
                            , question14'
                            ]
                      }
                }
        computed `shouldBe` expected
      it "Apply: EditQuestionEvent 2" $ do
        let event = e_km1_ch2_q4'
        let (Right computed) = compile (Just km1WithQ4) [EditQuestionEvent' event]
        let expected = putInQuestionsM question4Edited.uuid question4Edited' km1WithQ4
        computed `shouldBe` expected
      it "Apply: EditQuestionEvent 3" $ do
        let event = e_km1_ch2_q4'
        let (Right computed) = compile (Just km1WithQ4Plain) [EditQuestionEvent' event]
        let expected = putInQuestionsM question4Edited.uuid question4Edited' km1WithQ4Plain
        computed `shouldBe` expected
      it "Apply: DeleteQuestionEvent" $ do
        let initKM = setChaptersL km1 [chapter1WithAddedQuestion3, chapter2, chapter3]
        let (Right computed) = compile (Just initKM) [DeleteQuestionEvent' d_km1_ch1_q3]
        let expected =
              km1
                { entities =
                    km1.entities
                      { questions =
                          toMap
                            [ question1'
                            , question2'
                            , q2_aYes_fuQuestion1'
                            , q2_aYes_fuq1_aYes_fuQuestion2'
                            , question9'
                            , question10'
                            , question11'
                            , question12'
                            , question13'
                            , question14'
                            ]
                      , answers =
                          toMap
                            [ q2_answerNo
                            , q2_answerYes
                            , q2_aYes_fuq1_answerNo
                            , q2_aYes_fuq1_answerYes
                            , q2_aYes_fuq1_aYes_fuq2_answerNo
                            , q2_aYes_fuq1_aYes_fuq2_answerYes
                            ]
                      }
                }
        computed `shouldBe` expected
    -- ---------------
    describe "Apply: Answer Events" $ do
      it "Apply: AddAnswerEvent" $ do
        let (Right computed) = compile (Just km1) [AddAnswerEvent' a_km1_ch1_q2_aMaybe]
        let expected =
              putInQuestionsM question2.uuid (setAnswerUuids question2' [q2_answerNo.uuid, q2_answerYes.uuid, q2_answerMaybe.uuid]) $
                km1
                  { entities =
                      km1.entities
                        { answers =
                            toMap
                              [ q2_answerNo
                              , q2_answerYes
                              , q2_answerMaybe
                              , q2_aYes_fuq1_answerNo
                              , q2_aYes_fuq1_answerYes
                              , q2_aYes_fuq1_aYes_fuq2_answerNo
                              , q2_aYes_fuq1_aYes_fuq2_answerYes
                              , q3_answerNo
                              , q3_answerYes
                              ]
                        }
                  }
        computed `shouldBe` expected
      it "Apply: EditAnswerEvent" $ do
        let (Right computed) = compile (Just km1) [EditAnswerEvent' e_km1_ch1_q2_aYes1]
        let expected =
              km1
                { entities =
                    km1.entities
                      { answers =
                          toMap
                            [ q2_answerNo
                            , q2_answerYesEdited
                            , q2_aYes_fuq1_answerNo
                            , q2_aYes_fuq1_answerYes
                            , q2_aYes_fuq1_aYes_fuq2_answerNo
                            , q2_aYes_fuq1_aYes_fuq2_answerYes
                            , q3_answerNo
                            , q3_answerYes
                            ]
                      }
                }
        computed `shouldBe` expected
      it "Apply: DeleteAnswerEvent" $ do
        let (Right computed) = compile (Just km1) [DeleteAnswerEvent' d_km1_ch1_q2_aYes1]
        let expected =
              km1
                { entities =
                    km1.entities
                      { questions =
                          toMap
                            [ question1'
                            , setAnswerUuids question2' [q2_answerNo.uuid]
                            , question3'
                            , question9'
                            , question10'
                            , question11'
                            , question12'
                            , question13'
                            , question14'
                            ]
                      , answers = toMap [q2_answerNo, q3_answerNo, q3_answerYes]
                      }
                }
        computed `shouldBe` expected
    -- ---------------
    describe "Apply: Follow-Up Question Events" $ do
      it "Apply: AddFollowUpQuestionEvent" $ do
        let event = a_km1_ch1_q2_ansYes_fuq1_ansYes_fuq2_ansYes4_fuq3'
        let (Right computed) = compile (Just km1) [AddQuestionEvent' event]
        let expected =
              putInQuestionsM q2_aYes1_fuq1_aYes3_fuq2_aYes4_fuQuestion3.uuid q2_aYes1_fuq1_aYes3_fuq2_aYes4_fuQuestion3'
                . putInAnswersM q2_aYes_fuq1_aYes_fuq2_answerYes.uuid (q2_aYes_fuq1_aYes_fuq2_answerYes {followUpUuids = [q2_aYes1_fuq1_aYes3_fuq2_aYes4_fuQuestion3.uuid]})
                $ km1
        computed `shouldBe` expected
      it "Apply: EditFollowUpQuestionEvent" $ do
        let event = e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2'
        let (Right computed) = compile (Just km1) [EditQuestionEvent' event]
        let expected =
              putInQuestionsM q2_aYes_fuq1_aYes_fuQuestion2Edited.uuid q2_aYes_fuq1_aYes_fuQuestion2Edited' km1
        computed `shouldBe` expected
      it "Apply: DeleteFollowUpQuestionEvent" $ do
        let event = d_km1_ch1_ansYes1_fuq1_ansYes3_fuq2
        let (Right computed) = compile (Just km1) [DeleteQuestionEvent' event]
        let expected =
              km1
                { entities =
                    km1.entities
                      { questions = toMap $ L.delete q2_aYes_fuq1_aYes_fuQuestion2' (getQuestionsL km1)
                      , answers =
                          toMap
                            [ q2_answerNo
                            , q2_answerYes
                            , q2_aYes_fuq1_answerNo
                            , q2_aYes_fuq1_answerYes {followUpUuids = []}
                            , q3_answerNo
                            , q3_answerYes
                            ]
                      }
                }
        computed `shouldBe` expected
    -- ---------------
    describe "Apply: AnswerItemTemplateQuestion Events" $ do
      it "Apply: AddAnswerItemTemplateQuestionEvent" $ do
        let event = a_km1_ch2_q4_it1_q5'
        let (Right computed) = compile (Just km1WithQ4Plain) [AddQuestionEvent' event]
        let expected =
              putInQuestionsM q4_it1_question5Plain.uuid q4_it1_question5Plain'
                . putInQuestionsM question4.uuid (setItemTemplateQuestionUuids question4' [q4_it1_question5Plain.uuid])
                $ km1WithQ4Plain
        computed `shouldBe` expected
      it "Apply: EditAnswerItemTemplateQuestionEvent" $ do
        let event = e_km1_ch2_q4_it1_q5'
        let (Right computed) = compile (Just km1WithQ4) [EditQuestionEvent' event]
        let expected = putInQuestionsM q4_it1_question5Edited.uuid q4_it1_question5Edited' km1WithQ4
        computed `shouldBe` expected
      it "Apply: DeleteAnswerItemTemplateQuestionEvent" $ do
        let event = d_km1_ch2_q4_it1_q5
        let (Right computed) = compile (Just km1WithQ4) [DeleteQuestionEvent' event]
        let expected =
              km1WithQ4
                { entities =
                    km1WithQ4.entities
                      { questions =
                          toMap
                            [ question1'
                            , question2'
                            , q2_aYes_fuQuestion1'
                            , q2_aYes_fuq1_aYes_fuQuestion2'
                            , question3'
                            , setItemTemplateQuestionUuids question4' [q4_it1_question6.uuid]
                            , q4_it1_question6'
                            , q4_it1_q6_aYes_followUpQuestion4'
                            , q4_it1_q6_aYes_fuq4_it_question1'
                            , q4_it1_q6_aYes_fuq4_it_question2'
                            , q4_it1_q6_aYes_followUpQuestion5'
                            , question9'
                            , question10'
                            , question11'
                            , question12'
                            , question13'
                            , question14'
                            , question15'
                            ]
                      }
                }
        computed `shouldBe` expected
    -- ---------------
    describe "Apply: Choice Events" $ do
      it "Apply: AddChoiceEvent" $ do
        let (Right computed) = compile (Just km1) [AddChoiceEvent' a_km1_ch3_q11_cho3]
        let expected =
              putInQuestionsM question11.uuid (setChoiceUuids question11' [q11_choice1.uuid, q11_choice2.uuid, q11_choice3.uuid]) $
                km1
                  { entities =
                      km1.entities
                        { choices = toMap [q11_choice1, q11_choice2, q11_choice3]
                        }
                  }
        computed `shouldBe` expected
      it "Apply: EditChoiceEvent" $ do
        let (Right computed) = compile (Just km1) [EditChoiceEvent' e_km1_ch3_q11_cho1]
        let expected = setChoicesL km1 [q11_choice1Edited, q11_choice2]
        computed `shouldBe` expected
      it "Apply: DeleteChoiceEvent" $ do
        let (Right computed) = compile (Just km1) [DeleteChoiceEvent' d_km1_ch3_q11_cho1]
        let expected =
              putInQuestionsM question11.uuid (setChoiceUuids question11' [q11_choice2.uuid]) $
                km1
                  { entities =
                      km1.entities
                        { choices = toMap [q11_choice2]
                        }
                  }
        computed `shouldBe` expected
    -- ---------------
    describe "Apply: Expert Events" $ do
      it "Apply: AddExpertEvent" $ do
        let (Right computed) = compile (Just km1) [AddExpertEvent' a_km1_ch1_q2_eIsaac]
        let expected =
              putInQuestionsM question2.uuid (setExpertUuids question2' [km1_ch1_q2_eAlbert.uuid, km1_ch1_q2_eNikola.uuid, km1_ch1_q2_eIsaac.uuid]) $
                km1
                  { entities =
                      km1.entities
                        { experts = toMap [km1_ch1_q2_eAlbert, km1_ch1_q2_eNikola, km1_ch1_q2_eIsaac]
                        }
                  }
        computed `shouldBe` expected
      it "Apply: EditExpertEvent" $ do
        let (Right computed) = compile (Just km1) [EditExpertEvent' e_km1_ch1_q2_eAlbert]
        let expected = setExpertsL km1 [km1_ch1_q2_eAlbertEdited, km1_ch1_q2_eNikola]
        computed `shouldBe` expected
      it "Apply: DeleteExpertEvent" $ do
        let (Right computed) = compile (Just km1) [DeleteExpertEvent' d_km1_ch1_q2_eNikola]
        let expected =
              putInQuestionsM question2.uuid (setExpertUuids question2' [km1_ch1_q2_eAlbert.uuid]) $
                km1
                  { entities =
                      km1.entities
                        { experts = toMap [km1_ch1_q2_eAlbert]
                        }
                  }
        computed `shouldBe` expected
    -- ---------------
    describe "Apply: Reference Events" $ do
      it "Apply: AddReferenceEvent" $ do
        let (Right computed) = compile (Just km1) [AddReferenceEvent' a_km1_ch1_q2_rCh3']
        let expected =
              putInQuestionsM question2.uuid (setReferenceUuids question2' [km1_ch1_q2_r1.uuid, km1_ch1_q2_r2.uuid, km1_ch1_q2_r3.uuid]) $
                km1
                  { entities =
                      km1.entities
                        { references = toMap [km1_ch1_q2_r1', km1_ch1_q2_r2', km1_ch1_q2_r3']
                        }
                  }
        computed `shouldBe` expected
      it "Apply: EditReferenceEvent" $ do
        let (Right computed) = compile (Just km1) [EditReferenceEvent' e_km1_ch1_q2_rCh1']
        let expected = setReferencesL km1 [km1_ch1_q2_r1Edited', km1_ch1_q2_r2']
        computed `shouldBe` expected
      it "Apply: DeleteReferenceEvent" $ do
        let (Right computed) = compile (Just km1) [DeleteReferenceEvent' d_km1_ch1_q2_rCh2]
        let expected =
              putInQuestionsM question2.uuid (setReferenceUuids question2' [km1_ch1_q2_r1.uuid]) $
                km1
                  { entities =
                      km1.entities
                        { references = toMap [km1_ch1_q2_r1']
                        }
                  }
        computed `shouldBe` expected
    -- ---------------
    describe "Apply: Tag Events" $ do
      it "Apply: AddTagEvent" $ do
        let (Right computed) = compile (Just km1WithoutChaptersAndTagsAndIntegrations) [AddTagEvent' a_km1_tds]
        let expected =
              km1WithoutChaptersAndTagsAndIntegrations
                { tagUuids = [tagDataScience.uuid]
                , entities = km1WithoutChaptersAndTagsAndIntegrations.entities {tags = toMap [tagDataScience]}
                }
        computed `shouldBe` expected
      it "Apply: EditTagEvent" $ do
        let (Right computed) = compile (Just km1) [EditTagEvent' e_km1_tds]
        let expected = setTagsL km1 [tagDataScienceEdited, tagBioInformatic]
        computed `shouldBe` expected
      it "Apply: DeleteTagEvent" $ do
        let (Right computed) = compile (Just km1WithQ4) [DeleteTagEvent' d_km1_tds]
        let expected =
              putInQuestionsM question1.uuid (setTagUuids question1' [tagBioInformatic.uuid])
                . putInQuestionsM q2_aYes_fuQuestion1.uuid (setTagUuids q2_aYes_fuQuestion1' [])
                . putInQuestionsM q4_it1_question6.uuid (setTagUuids q4_it1_question6' [])
                $ km1WithQ4
                  { tagUuids = [tagBioInformatic.uuid]
                  , entities = km1WithQ4.entities {tags = toMap [tagBioInformatic]}
                  }
        computed `shouldBe` expected
    -- ---------------
    describe "Apply: Integration Events" $ do
      it "Apply: AddIntegrationEvent" $ do
        let (Right computed) = compile (Just km1WithoutChaptersAndTagsAndIntegrations) [AddIntegrationEvent' a_km1_iop']
        let expected =
              km1WithoutChaptersAndTagsAndIntegrations
                { integrationUuids = [ontologyPortal.uuid]
                , entities =
                    km1WithoutChaptersAndTagsAndIntegrations.entities
                      { integrations = toMap [ontologyPortal']
                      }
                }
        computed `shouldBe` expected
      it "Apply: EditIntegrationEvent" $ do
        let (Right computed) = compile (Just km1WithQ4) [EditIntegrationEvent' e_km1_iop']
        let expected =
              putInQuestionsM q4_it1_q6_aYes_fuq5VariablesEdited.uuid q4_it1_q6_aYes_fuq5VariablesEdited'
                . putInQuestionsM question9VariablesEdited.uuid question9VariablesEdited'
                $ km1WithQ4
                  { entities =
                      km1WithQ4.entities
                        { integrations = toMap [repositoryApi', ontologyPortalEdited', bioPortal', widgetPortal']
                        }
                  }
        computed `shouldBe` expected
      it "Apply: DeleteIntegrationEvent" $ do
        let (Right computed) = compile (Just km1WithQ4) [DeleteIntegrationEvent' d_km1_iop]
        let expected =
              putInQuestionsM q4_it1_q6_aYes_fuq5ConvertedToValue.uuid q4_it1_q6_aYes_fuq5ConvertedToValue'
                . putInQuestionsM question9ConvertedToValue.uuid question9ConvertedToValue'
                $ km1WithQ4
                  { integrationUuids = [repositoryApi.uuid, bioPortal.uuid, widgetPortal.uuid]
                  , entities = km1WithQ4.entities {integrations = toMap [repositoryApi', bioPortal', widgetPortal']}
                  }
        computed `shouldBe` expected
    -- ---------------
    describe "Apply: Resource Collection Events" $ do
      it "Apply: AddResourceCollectionEvent" $ do
        let (Right computed) = compile (Just km1WithoutChaptersAndTagsAndIntegrations) [AddResourceCollectionEvent' a_km1_rc1]
        let expected =
              km1WithoutChaptersAndTagsAndIntegrations
                { resourceCollectionUuids = [rc1.uuid]
                , entities =
                    km1WithoutChaptersAndTagsAndIntegrations.entities
                      { resourceCollections = toMap [rc1 {resourcePageUuids = []}]
                      }
                }
        computed `shouldBe` expected
      it "Apply: EditResourceCollectionEvent" $ do
        let (Right computed) = compile (Just km1WithQ4) [EditResourceCollectionEvent' e_km1_rc1]
        let expected =
              km1WithQ4
                { entities =
                    km1WithQ4.entities
                      { resourceCollections = toMap [rc1Edited, rc2]
                      }
                }
        computed `shouldBe` expected
      it "Apply: DeleteResourceCollectionnEvent" $ do
        let (Right computed) = compile (Just km1WithQ4) [DeleteResourceCollectionEvent' d_km1_rc1]
        let expected =
              km1WithQ4
                { resourceCollectionUuids = [rc2.uuid]
                , entities =
                    km1WithQ4.entities
                      { resourceCollections = toMap [rc2]
                      , resourcePages = toMap [rc2_rp1]
                      }
                }
        computed `shouldBe` expected
    -- ---------------
    describe "Apply: Resource Page Events" $ do
      it "Apply: AddResourcePageEvent" $ do
        let kmWithoutResourcePage =
              km1WithQ4
                { entities =
                    km1WithQ4.entities
                      { resourceCollections = toMap [rc1 {resourcePageUuids = [rc1_rp1.uuid]}, rc2]
                      , resourcePages = toMap [rc1_rp1, rc2_rp1]
                      }
                }
        let (Right computed) = compile (Just kmWithoutResourcePage) [AddResourcePageEvent' a_km1_rc1_rp2]
        let expected = km1WithQ4
        computed `shouldBe` expected
      it "Apply: EditResourcePageEvent" $ do
        let (Right computed) = compile (Just km1WithQ4) [EditResourcePageEvent' e_km1_rc1_rp1]
        let expected =
              km1WithQ4
                { entities =
                    km1WithQ4.entities
                      { resourcePages = toMap [rc1_rp1Edited, rc1_rp2, rc2_rp1]
                      }
                }
        computed `shouldBe` expected
      it "Apply: DeleteResourcePagenEvent" $ do
        let (Right computed) = compile (Just km1WithQ4) [DeleteResourcePageEvent' d_km1_rc1_rp1]
        let expected =
              km1WithQ4
                { entities =
                    km1WithQ4.entities
                      { resourceCollections = toMap [rc1 {resourcePageUuids = [rc1_rp2.uuid]}, rc2]
                      , resourcePages = toMap [rc1_rp2, rc2_rp1]
                      }
                }
        computed `shouldBe` expected
    -- ---------------
    describe "Apply: Move Events" $ do
      it "Apply: MoveQuestionEvent" $ do
        let (Right computed) = compile (Just km1) [MoveQuestionEvent' m_km1_ch1_q1__to_ch2]
        let expected =
              putInChaptersM chapter1.uuid (chapter1 {questionUuids = [question2.uuid]})
                . putInChaptersM chapter2.uuid (chapter2 {questionUuids = [question3.uuid, question1.uuid]})
                $ km1
        computed `shouldBe` expected
      it "Apply: MoveAnswerEvent" $ do
        let (Right computed) = compile (Just km1) [MoveAnswerEvent' m_km1_ch1_q2_aYes__to_ch2_q3]
        let expected =
              putInQuestionsM question2.uuid (setAnswerUuids question2' [q2_answerNo.uuid])
                . putInQuestionsM question3.uuid (setAnswerUuids question3' [q3_answerNo.uuid, q3_answerYes.uuid, q2_answerYes.uuid])
                $ km1
        computed `shouldBe` expected
      it "Apply: MoveChoiceEvent" $ do
        let (Right computed) = compile (Just km1) [MoveChoiceEvent' m_km1_ch3_q11_cho1__to_ch3_q12]
        let expected =
              putInQuestionsM question11.uuid (setChoiceUuids question11' [q11_choice2.uuid])
                . putInQuestionsM question12.uuid (setChoiceUuids question12' [q11_choice1.uuid])
                $ km1
        computed `shouldBe` expected
      it "Apply: MoveExpertEvent" $ do
        let (Right computed) = compile (Just km1) [MoveExpertEvent' m_km1_ch1_q2_eAlbert__to_ch2_q3]
        let expected =
              putInQuestionsM question2.uuid (setExpertUuids question2' [km1_ch1_q2_eNikola.uuid])
                . putInQuestionsM question3.uuid (setExpertUuids question3' [km1_ch1_q2_eAlbert.uuid])
                $ km1
        computed `shouldBe` expected
      it "Apply: MoveReferenceEvent" $ do
        let (Right computed) = compile (Just km1) [MoveReferenceEvent' m_km1_ch1_q2_r1__to_ch2_q3]
        let expected =
              putInQuestionsM question2.uuid (setReferenceUuids question2' [km1_ch1_q2_r2.uuid])
                . putInQuestionsM question3.uuid (setReferenceUuids question3' [km1_ch1_q2_r1.uuid])
                $ km1
        computed `shouldBe` expected
    -- ---------------
    describe "Build whole KM" $ it "Apply: Create KM from scratch" $ do
      let events =
            [ AddKnowledgeModelEvent' a_km1
            , AddMetricEvent' a_km1_mtrF
            , AddMetricEvent' a_km1_mtrA
            , AddMetricEvent' a_km1_mtrI
            , AddMetricEvent' a_km1_mtrR
            , AddMetricEvent' a_km1_mtrG
            , AddMetricEvent' a_km1_mtrO
            , AddPhaseEvent' a_km1_phs1
            , AddPhaseEvent' a_km1_phs2
            , AddPhaseEvent' a_km1_phs3
            , AddTagEvent' a_km1_tds
            , AddTagEvent' a_km1_tbi
            , AddIntegrationEvent' a_km1_ir'
            , AddIntegrationEvent' a_km1_iop'
            , AddIntegrationEvent' a_km1_ibp'
            , AddIntegrationEvent' a_km1_iwp'
            , AddResourceCollectionEvent' a_km1_rc1
            , AddResourcePageEvent' a_km1_rc1_rp1
            , AddResourcePageEvent' a_km1_rc1_rp2
            , AddResourceCollectionEvent' a_km1_rc2
            , AddResourcePageEvent' a_km1_rc2_rp1
            , AddChapterEvent' a_km1_ch1
            , AddQuestionEvent' a_km1_ch1_q1'
            , AddQuestionEvent' a_km1_ch1_q2'
            , AddAnswerEvent' a_km1_ch1_q2_aNo1
            , AddAnswerEvent' a_km1_ch1_q2_aYes1
            , AddQuestionEvent' a_km1_ch1_ansYes1_fuq1'
            , AddAnswerEvent' a_km1_ch1_q2_aYes1_fuq1_aNo
            , AddAnswerEvent' a_km1_ch1_q2_aYesFu1
            , AddQuestionEvent' a_km1_ch1_q2_ansYes_fuq1_ansYes_fuq2'
            , AddAnswerEvent' a_km1_ch1_q2_aNoFu2
            , AddAnswerEvent' a_km1_ch1_q2_aYesFu2
            , AddExpertEvent' a_km1_ch1_q2_eAlbert
            , AddExpertEvent' a_km1_ch1_q2_eNikola
            , AddReferenceEvent' a_km1_ch1_q2_rCh1'
            , AddReferenceEvent' a_km1_ch1_q2_rCh2'
            , AddChapterEvent' a_km1_ch2
            , AddQuestionEvent' a_km1_ch2_q3'
            , AddAnswerEvent' a_km1_ch2_q3_aNo2
            , AddAnswerEvent' a_km1_ch2_q3_aYes2
            , AddQuestionEvent' a_km1_ch2_q4'
            , AddQuestionEvent' a_km1_ch2_q4_it1_q5'
            , AddQuestionEvent' a_km1_ch2_q4_it1_q7'
            , AddQuestionEvent' a_km1_ch2_q4_it1_q8'
            , AddQuestionEvent' a_km1_ch2_q4_it1_q6'
            , AddAnswerEvent' a_km1_ch2_q4_it_q6_aNo
            , AddAnswerEvent' a_km1_ch2_q4_it_q6_aYes
            , AddQuestionEvent' a_km1_ch2_ansYes6_fuq4'
            , AddQuestionEvent' a_km1_ch2_ansYes6_fuq5'
            , AddQuestionEvent' a_km1_ch2_q4_it1_q6_fuq4_q1'
            , AddQuestionEvent' a_km1_ch2_q4_it1_q6_fuq4_q2'
            , AddExpertEvent' a_km1_ch2_q6_eAlbert
            , AddExpertEvent' a_km1_ch2_q6_eNikola
            , AddReferenceEvent' a_km1_ch2_q6_rCh1'
            , AddReferenceEvent' a_km1_ch2_q6_rCh2'
            , AddChapterEvent' a_km1_ch3
            , AddQuestionEvent' a_km1_ch3_q9'
            , AddQuestionEvent' a_km1_ch3_q10'
            , AddQuestionEvent' a_km1_ch3_q11'
            , AddChoiceEvent' a_km1_ch3_q11_cho1
            , AddChoiceEvent' a_km1_ch3_q11_cho2
            , AddQuestionEvent' a_km1_ch3_q12'
            , AddQuestionEvent' a_km1_ch3_q13'
            , AddQuestionEvent' a_km1_ch3_q14'
            , AddQuestionEvent' a_km1_ch3_q15'
            ]
      let (Right computed) = compile Nothing events
      let expected = km1WithQ4
      computed `shouldBe` expected
