module Shared.Specs.Model.KnowledgeModel.KnowledgeModelAccessorsSpec where

import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Shared.Database.Migration.Development.KnowledgeModel.Data.AnswersAndFollowUpQuestions
import Shared.Database.Migration.Development.KnowledgeModel.Data.Chapters
import Shared.Database.Migration.Development.KnowledgeModel.Data.Experts
import Shared.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Shared.Database.Migration.Development.KnowledgeModel.Data.Questions
import Shared.Database.Migration.Development.KnowledgeModel.Data.References
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelAccessors

knowledgeModelAccessorsSpec =
  describe "Knowledge Model Accessors" $
    ---------------------------------------------
    do
      describe "Question" $ do
        describe "getQuestionUuidsForChapterUuid" $
          it "Successfully listed" $
            getQuestionUuidsForChapterUuid km1 chapter1.uuid `shouldBe` [question1.uuid, question2.uuid]
        describe "getQuestionUuidsForAnswerUuid" $
          it "Successfully listed" $
            getQuestionUuidsForAnswerUuid km1 q2_answerYes.uuid `shouldBe` [q2_aYes_fuQuestion1.uuid]
      ---------------------------------------------
      describe "Expert" $
        describe "getExpertUuidsForQuestionUuid" $
          it "Successfully listed" $
            getExpertUuidsForQuestionUuid km1WithQ4 q4_it1_question6.uuid `shouldBe` [km1_ch2_q6_eAlbert.uuid, km1_ch2_q6_eNikola.uuid]
      ---------------------------------------------
      describe "Reference" $
        describe "getReferenceUuidsForQuestionUuid" $
          it "Successfully listed" $
            getReferenceUuidsForQuestionUuid km1WithQ4 q4_it1_question6.uuid `shouldBe` [km1_ch2_q6_r1.uuid, km1_ch2_q6_r2.uuid]
      ---------------------------------------------
      describe "Answer" $
        describe "getAnswerUuidsForQuestionUuid" $
          it "Successfully listed" $
            getAnswerUuidsForQuestionUuid km1 question2.uuid `shouldBe` [q2_answerNo.uuid, q2_answerYes.uuid]
