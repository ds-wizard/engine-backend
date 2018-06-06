module Specs.Service.Migrator.ApplicatorSpec where

import Control.Lens
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
import Model.Event.Event
import Service.Migrator.Applicator.Applicator

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
        let expAit = q4_aitChanged & questions .~ []
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
        let question2WithAddedAnswer = question2 & answers .~ (Just [q2_answerNo, q2_answerYes, q2_answerMaybe])
        let chapter1WithAddedAnswer = chapter1 & questions .~ [question1, question2WithAddedAnswer]
        let expected = km1 & chapters .~ [chapter1WithAddedAnswer, chapter2]
        computed `shouldBe` expected
      it "Apply:  EditAnswerEvent" $ do
        let (Right computed) = runApplicator (Just km1) [EditAnswerEvent' e_km1_ch1_q2_aYes1]
        let question2WithChangedAnswer = question2 & answers .~ (Just [q2_answerNo, q2_answerYesChanged])
        let chapter1WithChangedAnswer = chapter1 & questions .~ [question1, question2WithChangedAnswer]
        let expected = km1 & chapters .~ [chapter1WithChangedAnswer, chapter2]
        computed `shouldBe` expected
      it "Apply:  DeleteAnswerEvent" $ do
        let (Right computed) = runApplicator (Just km1) [DeleteAnswerEvent' d_km1_ch1_q2_aYes1]
        let question2WithDeletedAnswer = question2 & answers .~ (Just [q2_answerNo])
        let chapter1WithDeletedAnswer = chapter1 & questions .~ [question1, question2WithDeletedAnswer]
        let expected = km1 & chapters .~ [chapter1WithDeletedAnswer, chapter2]
        computed `shouldBe` expected
   -- ---------------
    describe "Apply:  Follow-Up Question Events" $ do
      it "Apply:  AddFollowUpQuestionEvent" $ do
        let event = a_km1_ch1_q2_ansYes_fuq1_ansYes_fuq2_ansYes4_fuq3
        let (Right computed) = runApplicator (Just km1) [AddQuestionEvent' event]
        let expFUQ3 = q2_aYes1_fuq1_aYes3_fuq2_aYes4_fuQuestion3
        let expq2_aYes_fuq1_aYes_fuq2_answerYes = q2_aYes_fuq1_aYes_fuq2_answerYes & followUps .~ [expFUQ3]
        let expFUQ2 =
              q2_aYes_fuq1_aYes_fuQuestion2 & answers .~
              (Just [q2_aYes_fuq1_aYes_fuq2_answerNo, expq2_aYes_fuq1_aYes_fuq2_answerYes])
        let expq2_aYes_fuq1_answerYes = q2_aYes_fuq1_answerYes & followUps .~ [expFUQ2]
        let expFUQ1 = q2_aYes_fuQuestion1 & answers .~ (Just [q2_aYes_fuq1_answerNo, expq2_aYes_fuq1_answerYes])
        let expQ2_AnswerYes = q2_answerYes & followUps .~ [expFUQ1]
        let expQuestion2 = question2 & answers .~ (Just [q2_answerNo, expQ2_AnswerYes])
        let expChapter1 = chapter1 & questions .~ [question1, expQuestion2]
        let expected = km1 & chapters .~ [expChapter1, chapter2]
        computed `shouldBe` expected
      it "Apply:  EditFollowUpQuestionEvent" $ do
        let event = e_km1_ch1_ansYes1_fuq1_ansYes3_fuq2
        let (Right computed) = runApplicator (Just km1) [EditQuestionEvent' event]
        let expFUQ2 = q2_aYes_fuq1_aYes_fuQuestion2Changed
        let expq2_aYes_fuq1_answerYes = q2_aYes_fuq1_answerYes & followUps .~ [expFUQ2]
        let expFUQ1 = q2_aYes_fuQuestion1 & answers .~ (Just [q2_aYes_fuq1_answerNo, expq2_aYes_fuq1_answerYes])
        let expQ2_AnswerYes = q2_answerYes & followUps .~ [expFUQ1]
        let expQuestion2 = question2 & answers .~ (Just [q2_answerNo, expQ2_AnswerYes])
        let expChapter1 = chapter1 & questions .~ [question1, expQuestion2]
        let expected = km1 & chapters .~ [expChapter1, chapter2]
        computed `shouldBe` expected
      it "Apply:  DeleteFollowUpQuestionEvent" $ do
        let event = d_km1_ch1_ansYes1_fuq1_ansYes3_fuq2
        let (Right computed) = runApplicator (Just km1) [DeleteQuestionEvent' event]
        let expq2_aYes_fuq1_answerYes = q2_aYes_fuq1_answerYes & followUps .~ []
        let expFUQ1 = q2_aYes_fuQuestion1 & answers .~ (Just [q2_aYes_fuq1_answerNo, expq2_aYes_fuq1_answerYes])
        let expQ2_AnswerYes = q2_answerYes & followUps .~ [expFUQ1]
        let expQuestion2 = question2 & answers .~ (Just [q2_answerNo, expQ2_AnswerYes])
        let expChapter1 = chapter1 & questions .~ [question1, expQuestion2]
        let expected = km1 & chapters .~ [expChapter1, chapter2]
        computed `shouldBe` expected
   -- ---------------
    describe "Apply:  AnswerItemTemplateQuestion Events" $ do
      it "Apply:  AddAnswerItemTemplateQuestionEvent" $ do
        let event = a_km1_ch2_q4_ait1_q5
        let (Right computed) = runApplicator (Just km1WithQ4Plain) [AddQuestionEvent' event]
        let expAit1 = q4_ait & questions .~ [q4_ait1_question5Plain]
        let expQuestion4 = question4 & answerItemTemplate .~ Just expAit1
        let expChapter2 = chapter2 & questions .~ [question3, expQuestion4]
        let expected = km1 & chapters .~ [chapter1, expChapter2]
        computed `shouldBe` expected
      it "Apply:  EditAnswerItemTemplateQuestionEvent" $ do
        let event = e_km1_ch2_q4_ait1_q5
        let (Right computed) = runApplicator (Just km1WithQ4) [EditQuestionEvent' event]
        let expAit1 = q4_ait & questions .~ [q4_ait1_question5Changed, q4_ait1_question6]
        let expQuestion4 = question4 & answerItemTemplate .~ Just expAit1
        let expChapter2 = chapter2 & questions .~ [question3, expQuestion4]
        let expected = km1 & chapters .~ [chapter1, expChapter2]
        computed `shouldBe` expected
      it "Apply:  DeleteAnswerItemTemplateQuestionEvent" $ do
        let event = d_km1_ch2_q4_ait1_q5
        let (Right computed) = runApplicator (Just km1WithQ4) [DeleteQuestionEvent' event]
        let expAit1 = q4_ait & questions .~ [q4_ait1_question6]
        let expQuestion4 = question4 & answerItemTemplate .~ Just expAit1
        let expChapter2 = chapter2 & questions .~ [question3, expQuestion4]
        let expected = km1 & chapters .~ [chapter1, expChapter2]
        computed `shouldBe` expected
   -- ---------------
    describe "Apply:  Expert Events" $ do
      it "Apply:  AddExpertEvent" $ do
        let (Right computed) = runApplicator (Just km1) [AddExpertEvent' a_km1_ch1_q2_eIsaac]
        let question2WithAddedExpert = question2 & experts .~ [expertAlbert, expertNikola, expertIsaac]
        let chapter1WithAddedExpert = chapter1 & questions .~ [question1, question2WithAddedExpert]
        let expected = km1 & chapters .~ [chapter1WithAddedExpert, chapter2]
        computed `shouldBe` expected
      it "Apply:  EditExpertEvent" $ do
        let (Right computed) = runApplicator (Just km1) [EditExpertEvent' e_km1_ch1_q2_eAlbert]
        let question2WithChangedExpert = question2 & experts .~ [expertAlbertChanged, expertNikola]
        let chapter1WithChangedExpert = chapter1 & questions .~ [question1, question2WithChangedExpert]
        let expected = km1 & chapters .~ [chapter1WithChangedExpert, chapter2]
        computed `shouldBe` expected
      it "Apply:  DeleteExpertEvent" $ do
        let (Right computed) = runApplicator (Just km1) [DeleteExpertEvent' d_km1_ch1_q2_eNikola]
        let question2WithDeletedExpert = question2 & experts .~ [expertAlbert]
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
              , AddQuestionEvent' a_km1_ch1_ansYes1_fuq1
              , AddAnswerEvent' a_km1_ch1_q2_aYes1_fuq1_aNo
              , AddAnswerEvent' a_km1_ch1_q2_aYesFu1
              , AddQuestionEvent' a_km1_ch1_q2_ansYes_fuq1_ansYes_fuq2
              , AddAnswerEvent' a_km1_ch1_q2_aNoFu2
              , AddAnswerEvent' a_km1_ch1_q2_aYesFu2
              , AddExpertEvent' a_km1_ch1_q2_eAlbert
              , AddExpertEvent' a_km1_ch1_q2_eNikola
              , AddReferenceEvent' a_km1_ch1_q2_rCh1
              , AddReferenceEvent' a_km1_ch1_q2_rCh2
              , AddChapterEvent' a_km1_ch2
              , AddQuestionEvent' a_km1_ch2_q3
              , AddAnswerEvent' a_km1_ch2_q3_aNo2
              , AddAnswerEvent' a_km1_ch2_q3_aYes2
              , AddQuestionEvent' a_km1_ch2_q4
              , AddQuestionEvent' a_km1_ch2_q4_ait1_q5
              , AddQuestionEvent' a_km1_ch2_q4_ait1_q7
              , AddQuestionEvent' a_km1_ch2_q4_ait1_q8
              , AddQuestionEvent' a_km1_ch2_q4_ait1_q6
              , AddAnswerEvent' a_km1_ch2_q4_ait_q6_aNo
              , AddAnswerEvent' a_km1_ch2_q4_ait_q6_aYes
              , AddQuestionEvent' a_km1_ch2_ansYes6_fuq4
              , AddQuestionEvent' a_km1_ch2_q4_ait1_q6_fuq4_q1
              , AddQuestionEvent' a_km1_ch2_q4_ait1_q6_fuq4_q2
              , AddExpertEvent' a_km1_ch2_q6_eAlbert
              , AddExpertEvent' a_km1_ch2_q6_eNikola
              , AddReferenceEvent' a_km1_ch2_q6_rCh1
              , AddReferenceEvent' a_km1_ch2_q6_rCh2
              ]
        let (Right computed) = runApplicator Nothing events
        let expected = km1WithQ4
        computed `shouldBe` expected
