module Specs.Model.KnowledgeModel.KnowledgeModelAccessorsSpec where

import Control.Lens
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Database.Migration.Development.KnowledgeModel.Data.AnswersAndFollowUpQuestions
import Database.Migration.Development.KnowledgeModel.Data.Chapters
import Database.Migration.Development.KnowledgeModel.Data.Experts
import Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Database.Migration.Development.KnowledgeModel.Data.Questions
import Database.Migration.Development.KnowledgeModel.Data.References
import LensesConfig
import Model.KnowledgeModel.KnowledgeModelAccessors

knowledgeModelAccessorsSpec =
  describe "Knowledge Model Accessors" $
    ---------------------------------------------
   do
    describe "Question" $ do
      describe "getQuestionUuidsForChapterUuid" $
        it "Successfully listed" $
        getQuestionUuidsForChapterUuid km1 (chapter1 ^. uuid) `shouldBe` [question1 ^. uuid, question2 ^. uuid]
      describe "getQuestionUuidsForAnswerUuid" $
        it "Successfully listed" $
        getQuestionUuidsForAnswerUuid km1 (q2_answerYes ^. uuid) `shouldBe` [q2_aYes_fuQuestion1 ^. uuid]
    ---------------------------------------------
    describe "Expert" $ do
      describe "getExpertUuidsForQuestionUuid" $
        it "Successfully listed" $
        getExpertUuidsForQuestionUuid km1WithQ4 (q4_it1_question6 ^. uuid) `shouldBe`
        [km1_ch2_q6_eAlbert ^. uuid, km1_ch2_q6_eNikola ^. uuid]
    ---------------------------------------------
    describe "Reference" $ do
      describe "getReferenceUuidsForQuestionUuid" $
        it "Successfully listed" $
        getReferenceUuidsForQuestionUuid km1WithQ4 (q4_it1_question6 ^. uuid) `shouldBe`
        [km1_ch2_q6_r1 ^. uuid, km1_ch2_q6_r2 ^. uuid]
    ---------------------------------------------
    describe "Answer" $ do
      describe "getAnswerUuidsForQuestionUuid" $
        it "Successfully listed" $
        getAnswerUuidsForQuestionUuid km1 (question2 ^. uuid) `shouldBe` [q2_answerNo ^. uuid, q2_answerYes ^. uuid]
