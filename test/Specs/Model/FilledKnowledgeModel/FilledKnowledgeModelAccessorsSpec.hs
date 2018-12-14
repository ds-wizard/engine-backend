module Specs.Model.FilledKnowledgeModel.FilledKnowledgeModelAccessorsSpec where

import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import qualified
       Database.Migration.Development.FilledKnowledgeModel.Data.FilledAnswersAndFollowUpQuestions
       as FA
import qualified
       Database.Migration.Development.FilledKnowledgeModel.Data.FilledChapters
       as FCH
import qualified
       Database.Migration.Development.FilledKnowledgeModel.Data.FilledQuestions
       as FQ
import Model.FilledKnowledgeModel.FilledKnowledgeModelAccessors

filledKnowledgeModelAccessorsSpec =
  describe "Filled Knowledge Model" $ do
    describe "getAllFilledQuestionsForChapter" $
      it "Successfully listed" $
      getAllFilledQuestionsForChapter FCH.fChapter2 `shouldBe`
      [ FQ.fQuestion3
      , FQ.fQuestion4
      , FA.fQ4_ait1_question5
      , FA.fQ4_ait1_question6
      , FA.fQ4_ait2_question5
      , FA.fQ4_ait2_question6
      , FA.fQ4_ait1_q5_ait1_question7
      , FA.fQ4_ait1_q5_ait1_question8
      ]
