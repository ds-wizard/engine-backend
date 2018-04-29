module Specs.Model.KnowledgeModel.KnowledgeModelAccessorsSpec where

import Control.Lens
import Data.Maybe
import qualified Data.UUID as U
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import qualified
       Database.Migration.Branch.Data.KnowledgeModel.AnswersAndFollowUpQuestions
       as FA
import qualified
       Database.Migration.Branch.Data.KnowledgeModel.Chapters as FCH
import qualified
       Database.Migration.Branch.Data.KnowledgeModel.Experts as FE
import qualified
       Database.Migration.Branch.Data.KnowledgeModel.KnowledgeModels
       as FKM
import qualified
       Database.Migration.Branch.Data.KnowledgeModel.Questions as FQ
import qualified
       Database.Migration.Branch.Data.KnowledgeModel.References as FR
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
      [FQ.question1, FQ.question2, FQ.question3, FA.followUpQuestion1, FA.followUpQuestion2]
    describe "getQuestionByUuid" $ it "Successfully listed" $ getQuestionByUuid FKM.km1 (FQ.question2 ^. uuid) `shouldBe`
      Just FQ.question2
    describe "getAllQuestionsForChapterUuid" $ it "Successfully listed" $
      getAllQuestionsForChapterUuid FKM.km1 (FCH.chapter1 ^. uuid) `shouldBe`
      [FQ.question1, FQ.question2]
    describe "getAllQuestionsForAnswerUuid" $ it "Successfully listed" $
      getAllQuestionsForAnswerUuid FKM.km1 (FA.answerYes1 ^. uuid) `shouldBe`
      [FA.followUpQuestion1]
    describe "isThereAnyQuestionWithGivenUuid" $ do
      it "Returns True if exists" $ isThereAnyQuestionWithGivenUuid FKM.km1 (FA.followUpQuestion1 ^. uuid) `shouldBe`
        True
      it "Returns False if not exists" $
        isThereAnyQuestionWithGivenUuid FKM.km1 (fromJust . U.fromString $ "c2dec208-3e58-473c-8cc3-a3964658e540") `shouldBe`
        False
    ---------------------------------------------
    describe "getAllAnswers" $ it "Successfully listed" $ getAllAnswers FKM.km1 `shouldBe`
      [ FA.answerNo1
      , FA.answerYes1
      , FA.answerNo2
      , FA.answerYes2
      , FA.answerNo3
      , FA.answerYes3
      , FA.answerNo4
      , FA.answerYes4
      ]
    describe "getAnswerByUuid" $ it "Successfully listed" $ getAnswerByUuid FKM.km1 (FA.answerNo3 ^. uuid) `shouldBe`
      Just FA.answerNo3
    describe "getAllAnswersForQuestionUuid" $ it "Successfully listed" $
      getAllAnswersForQuestionUuid FKM.km1 (FQ.question2 ^. uuid) `shouldBe`
      [FA.answerNo1, FA.answerYes1]
    describe "isThereAnyAnswerWithGivenUuid" $ do
      it "Returns True if exists" $ isThereAnyAnswerWithGivenUuid FKM.km1 (FA.answerYes3 ^. uuid) `shouldBe` True
      it "Returns False if not exists" $
        isThereAnyAnswerWithGivenUuid FKM.km1 (fromJust . U.fromString $ "c2dec208-3e58-473c-8cc3-a3964658e540") `shouldBe`
        False
    ---------------------------------------------
    describe "getAitQuestionIds" $ it "Successfully listed" $ getAitQuestionIds FQ.ait1 `shouldBe`
      [FQ.question5 ^. uuid, FQ.question6 ^. uuid]
    describe "aitChangeAitQuestionIdsOrder" $ it "Successfully changed" $ do
      let res = FQ.ait1 & aitChangeAitQuestionIdsOrder .~ [FQ.question6 ^. uuid, FQ.question5 ^. uuid]
      let exp = FQ.ait1 & questions .~ [FQ.question6, FQ.question5]
      res `shouldBe` exp
    describe "aitAnswerItemTemplatePlainWithIds" $ do
      it "Successfully changed" $ do
        let res =
              FQ.question4 & aitAnswerItemTemplatePlainWithIds .~
              Just
                AnswerItemTemplatePlainWithIds
                { _answerItemTemplatePlainWithIdsTitle = FQ.ait1WithChangeProperties ^. title
                , _answerItemTemplatePlainWithIdsQuestionIds =
                    FQ.ait1WithChangeProperties ^.. questions . traverse . uuid
                }
        let exp = FQ.question4 & answerItemTemplate .~ Just FQ.ait1WithChangeProperties
        res `shouldBe` exp
      it "Successfully created" $ do
        let reqPlainWithIdsChanged =
              AnswerItemTemplatePlainWithIds
              { _answerItemTemplatePlainWithIdsTitle = FQ.ait1WithChangeProperties ^. title
              , _answerItemTemplatePlainWithIdsQuestionIds = []
              }
        let res = FQ.question5 & aitAnswerItemTemplatePlainWithIds .~ Just reqPlainWithIdsChanged
        let expAit =
              AnswerItemTemplate
              {_answerItemTemplateTitle = FQ.ait1WithChangeProperties ^. title, _answerItemTemplateQuestions = []}
        let exp = FQ.question5 & answerItemTemplate .~ Just expAit
        res `shouldBe` exp
    ---------------------------------------------
    describe "getAllExperts" $ it "Successfully listed" $ getAllExperts FKM.km1 `shouldBe`
      [FE.expertDarth, FE.expertLuke]
    describe "getExpertByUuid" $ it "Successfully listed" $ getExpertByUuid FKM.km1 (FE.expertDarth ^. uuid) `shouldBe`
      Just FE.expertDarth
    describe "isThereAnyExpertWithGivenUuid" $ do
      it "Returns True if exists" $ isThereAnyExpertWithGivenUuid FKM.km1 (FE.expertDarth ^. uuid) `shouldBe` True
      it "Returns False if not exists" $
        isThereAnyExpertWithGivenUuid FKM.km1 (fromJust . U.fromString $ "c2dec208-3e58-473c-8cc3-a3964658e540") `shouldBe`
        False
    ---------------------------------------------
    describe "getAllReferences" $ it "Successfully listed" $ getAllReferences FKM.km1 `shouldBe`
      [FR.referenceCh1, FR.referenceCh2]
    describe "getReferenceByUuid" $ it "Successfully listed" $ getReferenceByUuid FKM.km1 (FR.referenceCh1 ^. uuid) `shouldBe`
      Just FR.referenceCh1
    describe "isThereAnyReferenceWithGivenUuid" $ do
      it "Returns True if exists" $ isThereAnyReferenceWithGivenUuid FKM.km1 (FR.referenceCh1 ^. uuid) `shouldBe` True
      it "Returns False if not exists" $
        isThereAnyReferenceWithGivenUuid FKM.km1 (fromJust . U.fromString $ "c2dec208-3e58-473c-8cc3-a3964658e540") `shouldBe`
        False
