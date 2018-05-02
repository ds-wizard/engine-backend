module Specs.Service.Migrator.ApplicatorSpec where

import Control.Lens
import Control.Monad.Reader (liftIO)
import Data.Maybe
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Database.Migration.Branch.Data.Event.Event
import Database.Migration.Branch.Data.KnowledgeModel.AnswersAndFollowUpQuestions
import Database.Migration.Branch.Data.KnowledgeModel.Chapters
import Database.Migration.Branch.Data.KnowledgeModel.Experts
import Database.Migration.Branch.Data.KnowledgeModel.KnowledgeModels
import Database.Migration.Branch.Data.KnowledgeModel.Questions
import Database.Migration.Branch.Data.KnowledgeModel.References
import LensesConfig
import Model.Common
import Model.Event.Chapter.ChapterEvent
import Model.Event.Event
import Model.Event.KnowledgeModel.KnowledgeModelEvent
import Model.KnowledgeModel.KnowledgeModel
import Service.Migrator.Applicator

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
        let expected = km1WithoutChapters
        computed `shouldBe` expected
      it "Apply:  EditKnowledgeEvent" $ do
        let (Right computed) = runApplicator (Just km1) [EditKnowledgeModelEvent' e_km1]
        let expected = km1WithChangeProperties
        computed `shouldBe` expected
   -- ---------------
    describe "Apply:  Chapter Events" $ do
      it "Apply:  AddChapterEvent" $ do
        let (Right computed) = runApplicator (Just km1) [AddChapterEvent' a_km1_ch3]
        let expected = km1 & chapters .~ [chapter1, chapter2, chapter3WithoutQuestions]
        computed `shouldBe` expected
      it "Apply:  EditChapterEvent" $ do
        let (Right computed) = runApplicator (Just km1) [EditChapterEvent' e_km1_ch1]
        let expected = km1 & chapters .~ [chapter1WithChangeProperties, chapter2]
        computed `shouldBe` expected
      it "Apply:  DeleteChapterEvent" $ do
        let (Right computed) = runApplicator (Just km1) [DeleteChapterEvent' d_km1_ch1]
        let expected = km1 & chapters .~ [chapter2]
        computed `shouldBe` expected
   -- ---------------
    describe "Apply:  Question Events" $ do
      it "Apply:  AddQuestionEvent" $ do
        let (Right computed) = runApplicator (Just km1) [AddQuestionEvent' a_km1_ch1_q3]
        let expected = km1 & chapters .~ [chapter1WithAddedQuestion3, chapter2]
        computed `shouldBe` expected
      it "Apply:  EditQuestionEvent" $ do
        let (Right computed) = runApplicator (Just km1) [EditQuestionEvent' e_km1_ch1_q2]
        let expected = km1 & chapters .~ [chapter1WithChangedQuestion2, chapter2]
        computed `shouldBe` expected
      it "Apply:  EditQuestionEvent 2" $ do
        let event = e_km1_ch2_q4
        let (Right computed) = runApplicator (Just km1WithQ4) [EditQuestionEvent' event]
        let expChapter2 = chapter2 & questions .~ [question3, question4WithChangeProperties]
        let expected = km1 & chapters .~ [chapter1, expChapter2]
        computed `shouldBe` expected
      it "Apply:  EditQuestionEvent 3" $ do
        let event = e_km1_ch2_q4
        let (Right computed) = runApplicator (Just km1WithQ4Plain) [EditQuestionEvent' event]
        let expAit = ait1WithChangeProperties & questions .~ []
        let expQuestion4 = question4WithChangeProperties & answerItemTemplate .~ Just expAit
        let expChapter2 = chapter2 & questions .~ [question3, expQuestion4]
        let expected = km1 & chapters .~ [chapter1, expChapter2]
        computed `shouldBe` expected
      it "Apply:  DeleteQuestionEvent" $ do
        let initKM = km1 & chapters .~ [chapter1WithAddedQuestion3, chapter2]
        let (Right computed) = runApplicator (Just initKM) [DeleteQuestionEvent' d_km1_ch1_q3]
        let expected = km1
        computed `shouldBe` expected
   -- ---------------
    describe "Apply:  Answer Events" $ do
      it "Apply:  AddAnswerEvent" $ do
        let (Right computed) = runApplicator (Just km1) [AddAnswerEvent' a_km1_ch1_q2_aMaybe]
        let question2WithAddedAnswer = question2 & answers .~ (Just [answerNo1, answerYes1, answerMaybe])
        let chapter1WithAddedAnswer = chapter1 & questions .~ [question1, question2WithAddedAnswer]
        let expected = km1 & chapters .~ [chapter1WithAddedAnswer, chapter2]
        computed `shouldBe` expected
      it "Apply:  EditAnswerEvent" $ do
        let (Right computed) = runApplicator (Just km1) [EditAnswerEvent' e_km1_ch1_q2_aYes1]
        let question2WithChangedAnswer = question2 & answers .~ (Just [answerNo1, answerYes1Changed])
        let chapter1WithChangedAnswer = chapter1 & questions .~ [question1, question2WithChangedAnswer]
        let expected = km1 & chapters .~ [chapter1WithChangedAnswer, chapter2]
        computed `shouldBe` expected
      it "Apply:  DeleteAnswerEvent" $ do
        let (Right computed) = runApplicator (Just km1) [DeleteAnswerEvent' d_km1_ch1_q2_aYes1]
        let question2WithDeletedAnswer = question2 & answers .~ (Just [answerNo1])
        let chapter1WithDeletedAnswer = chapter1 & questions .~ [question1, question2WithDeletedAnswer]
        let expected = km1 & chapters .~ [chapter1WithDeletedAnswer, chapter2]
        computed `shouldBe` expected
   -- ---------------
    describe "Apply:  Follow-Up Question Events" $ do
      it "Apply:  AddFollowUpQuestionEvent" $ do
        let event = a_km1_ch1_ansYes1_fuq1_ansYes3_fuq2_ansYes4_fuq3
        let (Right computed) = runApplicator (Just km1) [AddFollowUpQuestionEvent' event]
        let expFUQ3 = followUpQuestion3
        let expanswerYesFuq2 = answerYesFuq2 & followUps .~ [expFUQ3]
        let expFUQ2 = followUpQuestion2 & answers .~ (Just [answerNoFuq2, expanswerYesFuq2])
        let expanswerYesFuq1 = answerYesFuq1 & followUps .~ [expFUQ2]
        let expFUQ1 = followUpQuestion1 & answers .~ (Just [answerNoFuq1, expanswerYesFuq1])
        let expAnswerYes1 = answerYes1 & followUps .~ [expFUQ1]
        let expQuestion2 = question2 & answers .~ (Just [answerNo1, expAnswerYes1])
        let expChapter1 = chapter1 & questions .~ [question1, expQuestion2]
        let expected = km1 & chapters .~ [expChapter1, chapter2]
        computed `shouldBe` expected
      it "Apply:  EditFollowUpQuestionEvent" $ do
        let event = e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2
        let (Right computed) = runApplicator (Just km1) [EditFollowUpQuestionEvent' event]
        let expFUQ2 = followUpQuestion2Changed
        let expanswerYesFuq1 = answerYesFuq1 & followUps .~ [expFUQ2]
        let expFUQ1 = followUpQuestion1 & answers .~ (Just [answerNoFuq1, expanswerYesFuq1])
        let expAnswerYes1 = answerYes1 & followUps .~ [expFUQ1]
        let expQuestion2 = question2 & answers .~ (Just [answerNo1, expAnswerYes1])
        let expChapter1 = chapter1 & questions .~ [question1, expQuestion2]
        let expected = km1 & chapters .~ [expChapter1, chapter2]
        computed `shouldBe` expected
      it "Apply:  DeleteFollowUpQuestionEvent" $ do
        let event = d_km1_ch1_ansYes1_fuq1_ansYes3_fuq2
        let (Right computed) = runApplicator (Just km1) [DeleteFollowUpQuestionEvent' event]
        let expanswerYesFuq1 = answerYesFuq1 & followUps .~ []
        let expFUQ1 = followUpQuestion1 & answers .~ (Just [answerNoFuq1, expanswerYesFuq1])
        let expAnswerYes1 = answerYes1 & followUps .~ [expFUQ1]
        let expQuestion2 = question2 & answers .~ (Just [answerNo1, expAnswerYes1])
        let expChapter1 = chapter1 & questions .~ [question1, expQuestion2]
        let expected = km1 & chapters .~ [expChapter1, chapter2]
        computed `shouldBe` expected
   -- ---------------
    describe "Apply:  AnswerItemTemplateQuestion Events" $ do
      it "Apply:  AddAnswerItemTemplateQuestionEvent" $ do
        let event = a_km1_ch2_q4_ait1_q5
        let (Right computed) = runApplicator (Just km1WithQ4Plain) [AddAnswerItemTemplateQuestionEvent' event]
        let expAit1 = ait1 & questions .~ [question5Plain]
        let expQuestion4 = question4 & answerItemTemplate .~ Just expAit1
        let expChapter2 = chapter2 & questions .~ [question3, expQuestion4]
        let expected = km1 & chapters .~ [chapter1, expChapter2]
        computed `shouldBe` expected
      it "Apply:  EditAnswerItemTemplateQuestionEvent" $ do
        let event = e_km1_ch2_q4_ait1_q5
        let (Right computed) = runApplicator (Just km1WithQ4) [EditAnswerItemTemplateQuestionEvent' event]
        let expAit1 = ait1 & questions .~ [question5WithChangeProperties, question6]
        let expQuestion4 = question4 & answerItemTemplate .~ Just expAit1
        let expChapter2 = chapter2 & questions .~ [question3, expQuestion4]
        let expected = km1 & chapters .~ [chapter1, expChapter2]
        computed `shouldBe` expected
      it "Apply:  DeleteAnswerItemTemplateQuestionEvent" $ do
        let event = d_km1_ch2_q4_ait1
        let (Right computed) = runApplicator (Just km1WithQ4) [DeleteAnswerItemTemplateQuestionEvent' event]
        let expAit1 = ait1 & questions .~ [question6]
        let expQuestion4 = question4 & answerItemTemplate .~ Just expAit1
        let expChapter2 = chapter2 & questions .~ [question3, expQuestion4]
        let expected = km1 & chapters .~ [chapter1, expChapter2]
        computed `shouldBe` expected
   -- ---------------
    describe "Apply:  Expert Events" $ do
      it "Apply:  AddExpertEvent" $ do
        let (Right computed) = runApplicator (Just km1) [AddExpertEvent' a_km1_ch1_q2_eJohn]
        let question2WithAddedExpert = question2 & experts .~ [expertDarth, expertLuke, expertJohn]
        let chapter1WithAddedExpert = chapter1 & questions .~ [question1, question2WithAddedExpert]
        let expected = km1 & chapters .~ [chapter1WithAddedExpert, chapter2]
        computed `shouldBe` expected
      it "Apply:  EditExpertEvent" $ do
        let (Right computed) = runApplicator (Just km1) [EditExpertEvent' e_km1_ch1_q2_eDarth]
        let question2WithChangedExpert = question2 & experts .~ [expertDarthChanged, expertLuke]
        let chapter1WithChangedExpert = chapter1 & questions .~ [question1, question2WithChangedExpert]
        let expected = km1 & chapters .~ [chapter1WithChangedExpert, chapter2]
        computed `shouldBe` expected
      it "Apply:  DeleteExpertEvent" $ do
        let (Right computed) = runApplicator (Just km1) [DeleteExpertEvent' d_km1_ch1_q2_eLuke]
        let question2WithDeletedExpert = question2 & experts .~ [expertDarth]
        let chapter1WithDeletedExpert = chapter1 & questions .~ [question1, question2WithDeletedExpert]
        let expected = km1 & chapters .~ [chapter1WithDeletedExpert, chapter2]
        computed `shouldBe` expected
   -- ---------------
    describe "Apply:  Reference Events" $ do
      it "Apply:  AddReferenceEvent" $ do
        let (Right computed) = runApplicator (Just km1) [AddReferenceEvent' a_km1_ch1_q2_rCh3]
        let question2WithAddedReference = question2 & references .~ [referenceCh1, referenceCh2, referenceCh3]
        let chapter1WithAddedReference = chapter1 & questions .~ [question1, question2WithAddedReference]
        let expected = km1 & chapters .~ [chapter1WithAddedReference, chapter2]
        computed `shouldBe` expected
      it "Apply:  EditReferenceEvent" $ do
        let (Right computed) = runApplicator (Just km1) [EditReferenceEvent' e_km1_ch1_q2_rCh1]
        let question2WithChangedReference = question2 & references .~ [referenceCh1Changed, referenceCh2]
        let chapter1WithChangedReference = chapter1 & questions .~ [question1, question2WithChangedReference]
        let expected = km1 & chapters .~ [chapter1WithChangedReference, chapter2]
        computed `shouldBe` expected
      it "Apply:  DeleteReferenceEvent" $ do
        let (Right computed) = runApplicator (Just km1) [DeleteReferenceEvent' d_km1_ch1_q2_rCh2]
        let question2WithDeletedReference = question2 & references .~ [referenceCh1]
        let chapter1WithDeletedReference = chapter1 & questions .~ [question1, question2WithDeletedReference]
        let expected = km1 & chapters .~ [chapter1WithDeletedReference, chapter2]
        computed `shouldBe` expected
   -- ---------------
    describe "Build whole KM" $
      it "Apply: Create KM from scratch" $ do
        let events =
              [ AddKnowledgeModelEvent' a_km1
              , AddChapterEvent' a_km1_ch1
              , AddQuestionEvent' a_km1_ch1_q1
              , AddQuestionEvent' a_km1_ch1_q2
              , AddAnswerEvent' a_km1_ch1_q2_aNo1
              , AddAnswerEvent' a_km1_ch1_q2_aYes1
              , AddFollowUpQuestionEvent' a_km1_ch1_ansYes1_fuq1
              , AddAnswerEvent' a_km1_ch1_q2_aNoFu1
              , AddAnswerEvent' a_km1_ch1_q2_aYesFu1
              , AddFollowUpQuestionEvent' a_km1_ch1_ansYes1_fuq1_ansYes3_fuq2
              , AddAnswerEvent' a_km1_ch1_q2_aNoFu2
              , AddAnswerEvent' a_km1_ch1_q2_aYesFu2
              , AddExpertEvent' a_km1_ch1_q2_eDarth
              , AddExpertEvent' a_km1_ch1_q2_eLuke
              , AddReferenceEvent' a_km1_ch1_q2_rCh1
              , AddReferenceEvent' a_km1_ch1_q2_rCh2
              , AddChapterEvent' a_km1_ch2
              , AddQuestionEvent' a_km1_ch2_q3
              , AddAnswerEvent' a_km1_ch2_q3_aNo2
              , AddAnswerEvent' a_km1_ch2_q3_aYes2
              , AddQuestionEvent' a_km1_ch2_q4
              , AddAnswerItemTemplateQuestionEvent' a_km1_ch2_q4_ait1_q5
              , AddAnswerItemTemplateQuestionEvent' a_km1_ch2_q4_ait1_q7
              , AddAnswerItemTemplateQuestionEvent' a_km1_ch2_q4_ait1_q8
              , AddAnswerItemTemplateQuestionEvent' a_km1_ch2_q4_ait1_q6
              , AddAnswerEvent' a_km1_ch2_q6_aNo6
              , AddAnswerEvent' a_km1_ch2_q6_aYes6
              , AddFollowUpQuestionEvent' a_km1_ch2_ansYes6_fuq4
              , AddAnswerItemTemplateQuestionEvent' a_km1_ch2_q4_ait1_q6_fuq4_q1
              , AddAnswerItemTemplateQuestionEvent' a_km1_ch2_q4_ait1_q6_fuq4_q2
              , AddExpertEvent' a_km1_ch2_q6_eDarth
              , AddExpertEvent' a_km1_ch2_q6_eLuke
              , AddReferenceEvent' a_km1_ch2_q6_rCh1
              , AddReferenceEvent' a_km1_ch2_q6_rCh2
              ]
        let (Right computed) = runApplicator Nothing events
        let expected = km1WithQ4
        computed `shouldBe` expected
