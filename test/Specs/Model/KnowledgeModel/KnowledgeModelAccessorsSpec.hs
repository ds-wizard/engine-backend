module Specs.Model.KnowledgeModel.KnowledgeModelAccessorsSpec where

import Control.Lens
import Data.Maybe
import qualified Data.UUID as U
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import qualified
       Database.Migration.Development.KnowledgeModel.Data.AnswersAndFollowUpQuestions
       as FA
import qualified
       Database.Migration.Development.KnowledgeModel.Data.Chapters as FCH
import qualified
       Database.Migration.Development.KnowledgeModel.Data.Experts as FE
import qualified
       Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
       as FKM
import qualified
       Database.Migration.Development.KnowledgeModel.Data.Questions as FQ
import qualified
       Database.Migration.Development.KnowledgeModel.Data.References as FR
import LensesConfig
import Model.KnowledgeModel.KnowledgeModel
import Model.KnowledgeModel.KnowledgeModelAccessors

knowledgeModelAccessorsSpec =
  describe "Knowledge Model" $ do
    describe "getAllChapters" $ it "Successfully listed" $ getAllChapters FKM.km1 `shouldBe`
      [FCH.chapter1, FCH.chapter2]
    describe "getChapterByUuid" $ it "Successfully listed" $ getChapterByUuid FKM.km1 (FCH.chapter2 ^. uuid) `shouldBe`
      Just FCH.chapter2
    describe "isThereAnyChapterWithGivenUuid" $ do
      it "Returns True if exists" $ isThereAnyChapterWithGivenUuid FKM.km1 (FCH.chapter2 ^. uuid) `shouldBe` True
      it "Returns False if not exists" $
        isThereAnyChapterWithGivenUuid FKM.km1 (fromJust . U.fromString $ "c2dec208-3e58-473c-8cc3-a3964658e540") `shouldBe`
        False
    ---------------------------------------------
    describe "getAllQuestions" $ it "Successfully listed" $ getAllQuestions FKM.km1 `shouldBe`
      [FQ.question1, FQ.question2, FQ.question3, FA.q2_aYes_fuQuestion1, FA.q2_aYes_fuq1_aYes_fuQuestion2]
    describe "getQuestionByUuid" $ it "Successfully listed" $ getQuestionByUuid FKM.km1 (FQ.question2 ^. uuid) `shouldBe`
      Just FQ.question2
    describe "getAllQuestionsForChapterUuid" $ it "Successfully listed" $
      getAllQuestionsForChapterUuid FKM.km1 (FCH.chapter1 ^. uuid) `shouldBe`
      [FQ.question1, FQ.question2]
    describe "getAllQuestionsForAnswerUuid" $ it "Successfully listed" $
      getAllQuestionsForAnswerUuid FKM.km1 (FA.q2_answerYes ^. uuid) `shouldBe`
      [FA.q2_aYes_fuQuestion1]
    describe "isThereAnyQuestionWithGivenUuid" $ do
      it "Returns True if exists" $ isThereAnyQuestionWithGivenUuid FKM.km1 (FA.q2_aYes_fuQuestion1 ^. uuid) `shouldBe`
        True
      it "Returns False if not exists" $
        isThereAnyQuestionWithGivenUuid FKM.km1 (fromJust . U.fromString $ "c2dec208-3e58-473c-8cc3-a3964658e540") `shouldBe`
        False
    ---------------------------------------------
    describe "getAllAnswers" $ it "Successfully listed" $ getAllAnswers FKM.km1 `shouldBe`
      [ FA.q2_answerNo
      , FA.q2_answerYes
      , FA.q3_answerNo
      , FA.q3_answerYes
      , FA.q2_aYes_fuq1_answerNo
      , FA.q2_aYes_fuq1_answerYes
      , FA.q2_aYes_fuq1_aYes_fuq2_answerNo
      , FA.q2_aYes_fuq1_aYes_fuq2_answerYes
      ]
    describe "getAnswerByUuid" $ it "Successfully listed" $ getAnswerByUuid FKM.km1 (FA.q2_aYes_fuq1_answerNo ^. uuid) `shouldBe`
      Just FA.q2_aYes_fuq1_answerNo
    describe "getAllAnswersForQuestionUuid" $ it "Successfully listed" $
      getAllAnswersForQuestionUuid FKM.km1 (FQ.question2 ^. uuid) `shouldBe`
      [FA.q2_answerNo, FA.q2_answerYes]
    describe "isThereAnyAnswerWithGivenUuid" $ do
      it "Returns True if exists" $ isThereAnyAnswerWithGivenUuid FKM.km1 (FA.q2_aYes_fuq1_answerYes ^. uuid) `shouldBe`
        True
      it "Returns False if not exists" $
        isThereAnyAnswerWithGivenUuid FKM.km1 (fromJust . U.fromString $ "c2dec208-3e58-473c-8cc3-a3964658e540") `shouldBe`
        False
    ---------------------------------------------
    describe "getAitQuestionUuids" $ it "Successfully listed" $ getAitQuestionUuids FQ.q4_ait `shouldBe`
      [FQ.q4_ait1_question5 ^. uuid, FQ.q4_ait1_question6 ^. uuid]
    describe "aitChangeAitQuestionUuidsOrder" $ it "Successfully changed" $ do
      let res =
            FQ.q4_ait & aitChangeAitQuestionUuidsOrder .~ [FQ.q4_ait1_question6 ^. uuid, FQ.q4_ait1_question5 ^. uuid]
      let exp = FQ.q4_ait & questions .~ [FQ.q4_ait1_question6, FQ.q4_ait1_question5]
      res `shouldBe` exp
    describe "aitAnswerItemTemplatePlainWithUuids" $ do
      it "Successfully changed" $ do
        let res =
              FQ.question4 & aitAnswerItemTemplatePlainWithUuids .~
              Just
                AnswerItemTemplatePlainWithUuids
                { _answerItemTemplatePlainWithUuidsTitle = FQ.q4_aitChanged ^. title
                , _answerItemTemplatePlainWithUuidsQuestionUuids = FQ.q4_aitChanged ^.. questions . traverse . uuid
                }
        let exp = FQ.question4 & answerItemTemplate .~ Just FQ.q4_aitChanged
        res `shouldBe` exp
      it "Successfully created" $ do
        let reqPlainWithUuidsChanged =
              AnswerItemTemplatePlainWithUuids
              { _answerItemTemplatePlainWithUuidsTitle = FQ.q4_aitChanged ^. title
              , _answerItemTemplatePlainWithUuidsQuestionUuids = []
              }
        let res = FQ.q4_ait1_question5 & aitAnswerItemTemplatePlainWithUuids .~ Just reqPlainWithUuidsChanged
        let expAit =
              AnswerItemTemplate
              {_answerItemTemplateTitle = FQ.q4_aitChanged ^. title, _answerItemTemplateQuestions = []}
        let exp = FQ.q4_ait1_question5 & answerItemTemplate .~ Just expAit
        res `shouldBe` exp
    describe "getAllAnswerItemTemplates" $ it "Successfully listed" $ do
      let expected = [FQ.q4_ait, FQ.q4_ait_q5_ait, FA.q4_ait1_q6_aYes_fuq4_ait]
      let computed = getAllAnswerItemTemplates FKM.km1WithQ4
      computed `shouldBe` expected
    describe "getAllAitQuestionsForParentQuestionUuid" $ it "Successfully listed" $ do
      let expected = [FQ.q4_ait1_question5, FQ.q4_ait1_question6]
      let computed = getAllAitQuestionsForParentQuestionUuid FKM.km1WithQ4 (FQ.question4 ^. uuid)
      computed `shouldBe` expected
    ---------------------------------------------
    describe "getAllExperts" $ it "Successfully listed" $ getAllExperts FKM.km1 `shouldBe`
      [FE.expertAlbert, FE.expertNikola]
    describe "getExpertByUuid" $ it "Successfully listed" $ getExpertByUuid FKM.km1 (FE.expertAlbert ^. uuid) `shouldBe`
      Just FE.expertAlbert
    describe "isThereAnyExpertWithGivenUuid" $ do
      it "Returns True if exists" $ isThereAnyExpertWithGivenUuid FKM.km1 (FE.expertAlbert ^. uuid) `shouldBe` True
      it "Returns False if not exists" $
        isThereAnyExpertWithGivenUuid FKM.km1 (fromJust . U.fromString $ "c2dec208-3e58-473c-8cc3-a3964658e540") `shouldBe`
        False
    ---------------------------------------------
    describe "getAllReferences" $ it "Successfully listed" $ getAllReferences FKM.km1 `shouldBe`
      [FR.referenceCh1', FR.referenceCh2']
    describe "getReferenceByUuid" $ it "Successfully listed" $ getReferenceByUuid FKM.km1 (FR.referenceCh1 ^. uuid) `shouldBe`
      Just FR.referenceCh1'
    describe "isThereAnyReferenceWithGivenUuid" $ do
      it "Returns True if exists" $ isThereAnyReferenceWithGivenUuid FKM.km1 (FR.referenceCh1 ^. uuid) `shouldBe` True
      it "Returns False if not exists" $
        isThereAnyReferenceWithGivenUuid FKM.km1 (fromJust . U.fromString $ "c2dec208-3e58-473c-8cc3-a3964658e540") `shouldBe`
        False
