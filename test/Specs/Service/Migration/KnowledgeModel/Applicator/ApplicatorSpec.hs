module Specs.Service.Migration.KnowledgeModel.Applicator.ApplicatorSpec where

import Control.Lens
import Test.Hspec hiding (shouldBe)
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
import Model.KnowledgeModel.KnowledgeModel
import Service.Migration.KnowledgeModel.Applicator.Applicator

applicatorSpec =
  describe "Applicator" $ do
    describe "Apply: No events" $
      it "Apply: No events" $ do
        let emptyEvents = []
        let (Right computed) = runApplicator (Just km1) emptyEvents
        let expected = km1
        computed `shouldBe` expected
   -- ---------------
    describe "Apply:  KM Events" $ do
      it "Apply:  AddKnowledgeEvent" $ do
        let (Right computed) = runApplicator Nothing [AddKnowledgeModelEvent' a_km1]
        let expected = km1WithoutChaptersAndTagsAndIntegrations
        computed `shouldBe` expected
      it "Apply:  EditKnowledgeEvent" $ do
        let (Right computed) = runApplicator (Just km1) [EditKnowledgeModelEvent' e_km1]
        let expected = km1Edited
        computed `shouldBe` expected
   -- ---------------
    describe "Apply:  Chapter Events" $ do
      it "Apply:  AddChapterEvent" $ do
        let (Right computed) = runApplicator (Just km1) [AddChapterEvent' a_km1_ch4]
        let expected = km1 & chapters .~ [chapter1, chapter2, chapter3, chapter4WithoutQuestions]
        computed `shouldBe` expected
      it "Apply:  EditChapterEvent" $ do
        let (Right computed) = runApplicator (Just km1) [EditChapterEvent' e_km1_ch1]
        let expected = km1 & chapters .~ [chapter1Edited, chapter2, chapter3]
        computed `shouldBe` expected
      it "Apply:  DeleteChapterEvent" $ do
        let (Right computed) = runApplicator (Just km1) [DeleteChapterEvent' d_km1_ch1]
        let expected = km1 & chapters .~ [chapter2, chapter3]
        computed `shouldBe` expected
   -- ---------------
    describe "Apply:  Question Events" $ do
      it "Apply:  AddQuestionEvent" $ do
        let (Right computed) = runApplicator (Just km1) [AddQuestionEvent' a_km1_ch1_q3']
        let expected = km1 & chapters .~ [chapter1WithAddedQuestion3, chapter2, chapter3]
        computed `shouldBe` expected
      it "Apply:  EditQuestionEvent" $ do
        let (Right computed) = runApplicator (Just km1) [EditQuestionEvent' e_km1_ch1_q2']
        let expected = km1 & chapters .~ [chapter1WithChangedQuestion2, chapter2, chapter3]
        computed `shouldBe` expected
      it "Apply:  EditQuestionEvent 2" $ do
        let event = e_km1_ch2_q4'
        let (Right computed) = runApplicator (Just km1WithQ4) [EditQuestionEvent' event]
        let expChapter2 = chapter2 & questions .~ [question3', question4Edited']
        let expected = km1 & chapters .~ [chapter1, expChapter2, chapter3]
        computed `shouldBe` expected
      it "Apply:  EditQuestionEvent 3" $ do
        let event = e_km1_ch2_q4'
        let (Right computed) = runApplicator (Just km1WithQ4Plain) [EditQuestionEvent' event]
        let expQuestion4 = question4Edited & itemTemplateQuestions .~ []
        let expChapter2 = chapter2 & questions .~ [question3', ListQuestion' expQuestion4]
        let expected = km1 & chapters .~ [chapter1, expChapter2, chapter3]
        computed `shouldBe` expected
      it "Apply:  DeleteQuestionEvent" $ do
        let initKM = km1 & chapters .~ [chapter1WithAddedQuestion3, chapter2, chapter3]
        let (Right computed) = runApplicator (Just initKM) [DeleteQuestionEvent' d_km1_ch1_q3]
        let expected = km1
        computed `shouldBe` expected
   -- ---------------
    describe "Apply:  Answer Events" $ do
      it "Apply:  AddAnswerEvent" $ do
        let (Right computed) = runApplicator (Just km1) [AddAnswerEvent' a_km1_ch1_q2_aMaybe]
        let question2WithAddedAnswer = question2 & answers .~ [q2_answerNo, q2_answerYes, q2_answerMaybe]
        let chapter1WithAddedAnswer = chapter1 & questions .~ [question1', OptionsQuestion' question2WithAddedAnswer]
        let expected = km1 & chapters .~ [chapter1WithAddedAnswer, chapter2, chapter3]
        computed `shouldBe` expected
      it "Apply:  EditAnswerEvent" $ do
        let (Right computed) = runApplicator (Just km1) [EditAnswerEvent' e_km1_ch1_q2_aYes1]
        let question2WithChangedAnswer = question2 & answers .~ [q2_answerNo, q2_answerYesEdited]
        let chapter1WithChangedAnswer =
              chapter1 & questions .~ [question1', OptionsQuestion' question2WithChangedAnswer]
        let expected = km1 & chapters .~ [chapter1WithChangedAnswer, chapter2, chapter3]
        computed `shouldBe` expected
      it "Apply:  DeleteAnswerEvent" $ do
        let (Right computed) = runApplicator (Just km1) [DeleteAnswerEvent' d_km1_ch1_q2_aYes1]
        let question2WithDeletedAnswer = question2 & answers .~ [q2_answerNo]
        let chapter1WithDeletedAnswer =
              chapter1 & questions .~ [question1', OptionsQuestion' question2WithDeletedAnswer]
        let expected = km1 & chapters .~ [chapter1WithDeletedAnswer, chapter2, chapter3]
        computed `shouldBe` expected
   -- ---------------
    describe "Apply:  Follow-Up Question Events" $ do
      it "Apply:  AddFollowUpQuestionEvent" $ do
        let event = a_km1_ch1_q2_ansYes_fuq1_ansYes_fuq2_ansYes4_fuq3'
        let (Right computed) = runApplicator (Just km1) [AddQuestionEvent' event]
        let expFUQ3 = q2_aYes1_fuq1_aYes3_fuq2_aYes4_fuQuestion3'
        let expq2_aYes_fuq1_aYes_fuq2_answerYes = q2_aYes_fuq1_aYes_fuq2_answerYes & followUps .~ [expFUQ3]
        let expFUQ2 =
              q2_aYes_fuq1_aYes_fuQuestion2 & answers .~
              [q2_aYes_fuq1_aYes_fuq2_answerNo, expq2_aYes_fuq1_aYes_fuq2_answerYes]
        let expq2_aYes_fuq1_answerYes = q2_aYes_fuq1_answerYes & followUps .~ [OptionsQuestion' expFUQ2]
        let expFUQ1 = q2_aYes_fuQuestion1 & answers .~ [q2_aYes_fuq1_answerNo, expq2_aYes_fuq1_answerYes]
        let expQ2_AnswerYes = q2_answerYes & followUps .~ [OptionsQuestion' expFUQ1]
        let expQuestion2 = question2 & answers .~ [q2_answerNo, expQ2_AnswerYes]
        let expChapter1 = chapter1 & questions .~ [question1', OptionsQuestion' expQuestion2]
        let expected = km1 & chapters .~ [expChapter1, chapter2, chapter3]
        computed `shouldBe` expected
      it "Apply:  EditFollowUpQuestionEvent" $ do
        let event = e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2'
        let (Right computed) = runApplicator (Just km1) [EditQuestionEvent' event]
        let expFUQ2 = q2_aYes_fuq1_aYes_fuQuestion2Edited'
        let expq2_aYes_fuq1_answerYes = q2_aYes_fuq1_answerYes & followUps .~ [expFUQ2]
        let expFUQ1 = q2_aYes_fuQuestion1 & answers .~ [q2_aYes_fuq1_answerNo, expq2_aYes_fuq1_answerYes]
        let expQ2_AnswerYes = q2_answerYes & followUps .~ [OptionsQuestion' expFUQ1]
        let expQuestion2 = question2 & answers .~ [q2_answerNo, expQ2_AnswerYes]
        let expChapter1 = chapter1 & questions .~ [question1', OptionsQuestion' expQuestion2]
        let expected = km1 & chapters .~ [expChapter1, chapter2, chapter3]
        computed `shouldBe` expected
      it "Apply:  DeleteFollowUpQuestionEvent" $ do
        let event = d_km1_ch1_ansYes1_fuq1_ansYes3_fuq2
        let (Right computed) = runApplicator (Just km1) [DeleteQuestionEvent' event]
        let expq2_aYes_fuq1_answerYes = q2_aYes_fuq1_answerYes & followUps .~ []
        let expFUQ1 = q2_aYes_fuQuestion1 & answers .~ [q2_aYes_fuq1_answerNo, expq2_aYes_fuq1_answerYes]
        let expQ2_AnswerYes = q2_answerYes & followUps .~ [OptionsQuestion' expFUQ1]
        let expQuestion2 = question2 & answers .~ [q2_answerNo, expQ2_AnswerYes]
        let expChapter1 = chapter1 & questions .~ [question1', OptionsQuestion' expQuestion2]
        let expected = km1 & chapters .~ [expChapter1, chapter2, chapter3]
        computed `shouldBe` expected
   -- ---------------
    describe "Apply:  AnswerItemTemplateQuestion Events" $ do
      it "Apply:  AddAnswerItemTemplateQuestionEvent" $ do
        let event = a_km1_ch2_q4_it1_q5'
        let (Right computed) = runApplicator (Just km1WithQ4Plain) [AddQuestionEvent' event]
        let expQuestion4 = question4 & itemTemplateQuestions .~ [q4_it1_question5Plain']
        let expChapter2 = chapter2 & questions .~ [question3', ListQuestion' expQuestion4]
        let expected = km1 & chapters .~ [chapter1, expChapter2, chapter3]
        computed `shouldBe` expected
      it "Apply:  EditAnswerItemTemplateQuestionEvent" $ do
        let event = e_km1_ch2_q4_it1_q5'
        let (Right computed) = runApplicator (Just km1WithQ4) [EditQuestionEvent' event]
        let expQuestion4 = question4 & itemTemplateQuestions .~ [q4_it1_question5Edited', q4_it1_question6']
        let expChapter2 = chapter2 & questions .~ [question3', ListQuestion' expQuestion4]
        let expected = km1 & chapters .~ [chapter1, expChapter2, chapter3]
        computed `shouldBe` expected
      it "Apply:  DeleteAnswerItemTemplateQuestionEvent" $ do
        let event = d_km1_ch2_q4_it1_q5
        let (Right computed) = runApplicator (Just km1WithQ4) [DeleteQuestionEvent' event]
        let expQuestion4 = question4 & itemTemplateQuestions .~ [OptionsQuestion' q4_it1_question6]
        let expChapter2 = chapter2 & questions .~ [question3', ListQuestion' expQuestion4]
        let expected = km1 & chapters .~ [chapter1, expChapter2, chapter3]
        computed `shouldBe` expected
   -- ---------------
    describe "Apply:  Expert Events" $ do
      it "Apply:  AddExpertEvent" $ do
        let (Right computed) = runApplicator (Just km1) [AddExpertEvent' a_km1_ch1_q2_eIsaac]
        let question2WithAddedExpert = question2 & experts .~ [expertAlbert, expertNikola, expertIsaac]
        let chapter1WithAddedExpert = chapter1 & questions .~ [question1', OptionsQuestion' question2WithAddedExpert]
        let expected = km1 & chapters .~ [chapter1WithAddedExpert, chapter2, chapter3]
        computed `shouldBe` expected
      it "Apply:  EditExpertEvent" $ do
        let (Right computed) = runApplicator (Just km1) [EditExpertEvent' e_km1_ch1_q2_eAlbert]
        let question2WithChangedExpert = question2 & experts .~ [expertAlbertEdited, expertNikola]
        let chapter1WithChangedExpert =
              chapter1 & questions .~ [question1', OptionsQuestion' question2WithChangedExpert]
        let expected = km1 & chapters .~ [chapter1WithChangedExpert, chapter2, chapter3]
        computed `shouldBe` expected
      it "Apply:  DeleteExpertEvent" $ do
        let (Right computed) = runApplicator (Just km1) [DeleteExpertEvent' d_km1_ch1_q2_eNikola]
        let question2WithDeletedExpert = question2 & experts .~ [expertAlbert]
        let chapter1WithDeletedExpert =
              chapter1 & questions .~ [question1', OptionsQuestion' question2WithDeletedExpert]
        let expected = km1 & chapters .~ [chapter1WithDeletedExpert, chapter2, chapter3]
        computed `shouldBe` expected
   -- ---------------
    describe "Apply:  Reference Events" $ do
      it "Apply:  AddReferenceEvent" $ do
        let (Right computed) = runApplicator (Just km1) [AddReferenceEvent' a_km1_ch1_q2_rCh3']
        let question2WithAddedReference = question2 & references .~ [referenceCh1', referenceCh2', referenceCh3']
        let chapter1WithAddedReference =
              chapter1 & questions .~ [question1', OptionsQuestion' question2WithAddedReference]
        let expected = km1 & chapters .~ [chapter1WithAddedReference, chapter2, chapter3]
        computed `shouldBe` expected
      it "Apply:  EditReferenceEvent" $ do
        let (Right computed) = runApplicator (Just km1) [EditReferenceEvent' e_km1_ch1_q2_rCh1']
        let question2WithChangedReference = question2 & references .~ [referenceCh1Edited', referenceCh2']
        let chapter1WithChangedReference =
              chapter1 & questions .~ [question1', OptionsQuestion' question2WithChangedReference]
        let expected = km1 & chapters .~ [chapter1WithChangedReference, chapter2, chapter3]
        computed `shouldBe` expected
      it "Apply:  DeleteReferenceEvent" $ do
        let (Right computed) = runApplicator (Just km1) [DeleteReferenceEvent' d_km1_ch1_q2_rCh2]
        let question2WithDeletedReference = question2 & references .~ [referenceCh1']
        let chapter1WithDeletedReference =
              chapter1 & questions .~ [question1', OptionsQuestion' question2WithDeletedReference]
        let expected = km1 & chapters .~ [chapter1WithDeletedReference, chapter2, chapter3]
        computed `shouldBe` expected
   -- ---------------
    describe "Apply:  Tag Events" $ do
      it "Apply:  AddTagEvent" $ do
        let (Right computed) = runApplicator (Just km1WithoutChaptersAndTagsAndIntegrations) [AddTagEvent' a_km1_tds]
        let expected = km1WithoutChaptersAndTagsAndIntegrations & tags .~ [tagDataScience]
        computed `shouldBe` expected
      it "Apply:  EditTagEvent" $ do
        let (Right computed) = runApplicator (Just km1) [EditTagEvent' e_km1_tds]
        let expected = km1 & tags .~ [tagDataScienceEdited, tagBioInformatic]
        computed `shouldBe` expected
      it "Apply:  DeleteTagEvent" $ do
        let (Right computed) = runApplicator (Just km1WithQ4) [DeleteTagEvent' d_km1_tds]
        -- Chapter 1
        let expQuestion1 = question1 & tagUuids .~ []
        let expQ2_aYes_fuQuestion1 = q2_aYes_fuQuestion1 & tagUuids .~ []
        let expQ2_answerYes = q2_answerYes & followUps .~ [OptionsQuestion' expQ2_aYes_fuQuestion1]
        let expQuestion2 = question2 & answers .~ [q2_answerNo, expQ2_answerYes]
        let expChapter1 = chapter1 & questions .~ [ValueQuestion' expQuestion1, OptionsQuestion' expQuestion2]
        -- Chapter 2
        let expQ4_it1_question6 = q4_it1_question6 & tagUuids .~ []
        let expQuestion4 =
              question4 & itemTemplateQuestions .~ [q4_it1_question5', OptionsQuestion' expQ4_it1_question6]
        let expChapter2 = chapter2 & questions .~ [question3', ListQuestion' expQuestion4]
        -- KM
        let expected = (km1WithQ4 & chapters .~ [expChapter1, expChapter2, chapter3]) & tags .~ [tagBioInformatic]
        computed `shouldBe` expected
   -- ---------------
    describe "Apply:  Integration Events" $ do
      it "Apply:  AddIntegrationEvent" $ do
        let (Right computed) =
              runApplicator (Just km1WithoutChaptersAndTagsAndIntegrations) [AddIntegrationEvent' a_km1_iop]
        let expected = km1WithoutChaptersAndTagsAndIntegrations & integrations .~ [ontologyPortal]
        computed `shouldBe` expected
      it "Apply:  EditIntegrationEvent" $ do
        let (Right computed) = runApplicator (Just km1WithQ4) [EditIntegrationEvent' e_km1_iop]
        -- Chapter 2
        let expQ4_it1_q6_answerYes =
              q4_it1_q6_answerYes & followUps .~ [q4_it1_q6_aYes_followUpQuestion4', q4_it1_q6_aYes_fuq5PropsEdited']
        let expQ4_it1_question6 = q4_it1_question6 & answers .~ [q4_it1_q6_answerNo, expQ4_it1_q6_answerYes]
        let expQuestion4 =
              question4 & itemTemplateQuestions .~ [q4_it1_question5', OptionsQuestion' expQ4_it1_question6]
        let expChapter2 = chapter2WithQ4 & questions .~ [question3', ListQuestion' expQuestion4]
        -- Chapter 3
        let expChapter3 = chapter3 & questions .~ [question9PropsEdited', question10']
        -- KM
        let expected =
              (km1WithQ4 & chapters .~ [chapter1, expChapter2, expChapter3]) & integrations .~
              [ontologyPortalEdited, bioPortal]
        computed `shouldBe` expected
      it "Apply:  DeleteIntegrationEvent" $ do
        let (Right computed) = runApplicator (Just km1WithQ4) [DeleteIntegrationEvent' d_km1_iop]
        -- Chapter 2
        let expQ4_it1_q6_answerYes =
              q4_it1_q6_answerYes & followUps .~
              [q4_it1_q6_aYes_followUpQuestion4', q4_it1_q6_aYes_fuq5ConvertedToValue']
        let expQ4_it1_question6 = q4_it1_question6 & answers .~ [q4_it1_q6_answerNo, expQ4_it1_q6_answerYes]
        let expQuestion4 =
              question4 & itemTemplateQuestions .~ [q4_it1_question5', OptionsQuestion' expQ4_it1_question6]
        let expChapter2 = chapter2WithQ4 & questions .~ [question3', ListQuestion' expQuestion4]
        -- Chapter 3
        let expChapter3 = chapter3 & questions .~ [question9ConvertedToValue', question10ConvertedToValue']
        -- KM
        let expected = (km1WithQ4 & chapters .~ [chapter1, expChapter2, expChapter3]) & integrations .~ [bioPortal]
        computed `shouldBe` expected
   -- ---------------
    describe "Build whole KM" $
      it "Apply: Create KM from scratch" $ do
        let events =
              [ AddKnowledgeModelEvent' a_km1
              , AddTagEvent' a_km1_tds
              , AddTagEvent' a_km1_tbi
              , AddIntegrationEvent' a_km1_iop
              , AddIntegrationEvent' a_km1_ibp
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
              ]
        let (Right computed) = runApplicator Nothing events
        let expected = km1WithQ4
        computed `shouldBe` expected
