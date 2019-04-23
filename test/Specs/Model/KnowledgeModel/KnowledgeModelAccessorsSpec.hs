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
       Database.Migration.Development.KnowledgeModel.Data.Integrations
       as FI
import qualified
       Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
       as FKM
import qualified
       Database.Migration.Development.KnowledgeModel.Data.Questions as FQ
import qualified
       Database.Migration.Development.KnowledgeModel.Data.References as FR
import qualified
       Database.Migration.Development.KnowledgeModel.Data.Tags as FT
import LensesConfig
import Model.KnowledgeModel.KnowledgeModelAccessors

knowledgeModelAccessorsSpec =
  describe "Knowledge Model Accessors" $ do
    describe "Chapter" $ do
      describe "getAllChapters" $
        it "Successfully listed" $ getAllChapters FKM.km1 `shouldBe` [FCH.chapter1, FCH.chapter2, FCH.chapter3]
      describe "getChapterByUuid" $
        it "Successfully listed" $ getChapterByUuid FKM.km1 (FCH.chapter2 ^. uuid) `shouldBe` Just FCH.chapter2
      describe "isThereAnyChapterWithGivenUuid" $ do
        it "Returns True if exists" $ isThereAnyChapterWithGivenUuid FKM.km1 (FCH.chapter2 ^. uuid) `shouldBe` True
        it "Returns False if not exists" $
          isThereAnyChapterWithGivenUuid FKM.km1 (fromJust . U.fromString $ "c2dec208-3e58-473c-8cc3-a3964658e540") `shouldBe`
          False
    ---------------------------------------------
    describe "Tag" $ do
      describe "getAllTags" $
        it "Successfully listed" $ getAllTags FKM.km1 `shouldBe` [FT.tagDataScience, FT.tagBioInformatic]
      describe "getTagByUuid" $
        it "Successfully listed" $
        getTagByUuid FKM.km1 (FT.tagBioInformatic ^. uuid) `shouldBe` Just FT.tagBioInformatic
      describe "isThereAnyTagWithGivenUuid" $ do
        it "Returns True if exists" $ isThereAnyTagWithGivenUuid FKM.km1 (FT.tagBioInformatic ^. uuid) `shouldBe` True
        it "Returns False if not exists" $
          isThereAnyTagWithGivenUuid FKM.km1 (fromJust . U.fromString $ "c2dec208-3e58-473c-8cc3-a3964658e540") `shouldBe`
          False
    ---------------------------------------------
    describe "Integration" $ do
      describe "getAllIntegrations" $
        it "Successfully listed" $ getAllIntegrations FKM.km1 `shouldBe` [FI.ontologyPortal, FI.bioPortal]
      describe "getIntegrationByUuid" $
        it "Successfully listed" $ getIntegrationByUuid FKM.km1 (FI.bioPortal ^. uuid) `shouldBe` Just FI.bioPortal
      describe "isThereAnyIntegrationWithGivenUuid" $ do
        it "Returns True if exists" $ isThereAnyIntegrationWithGivenUuid FKM.km1 (FI.bioPortal ^. uuid) `shouldBe` True
        it "Returns False if not exists" $
          isThereAnyTagWithGivenUuid FKM.km1 (fromJust . U.fromString $ "c2dec208-3e58-473c-8cc3-a3964658e540") `shouldBe`
          False
    ---------------------------------------------
    describe "Question" $ do
      describe "getAllQuestions" $
        it "Successfully listed" $
        getAllQuestions FKM.km1 `shouldBe`
        [ FQ.question1'
        , FQ.question2'
        , FQ.question3'
        , FQ.question9'
        , FQ.question10'
        , FA.q2_aYes_fuQuestion1'
        , FA.q2_aYes_fuq1_aYes_fuQuestion2'
        ]
      describe "getQuestionByUuid" $
        it "Successfully listed" $ getQuestionByUuid FKM.km1 (FQ.question2 ^. uuid) `shouldBe` Just FQ.question2'
      describe "getAllQuestionsForChapterUuid" $
        it "Successfully listed" $
        getAllQuestionsForChapterUuid FKM.km1 (FCH.chapter1 ^. uuid) `shouldBe` [FQ.question1', FQ.question2']
      describe "getAllQuestionsForAnswerUuid" $
        it "Successfully listed" $
        getAllQuestionsForAnswerUuid FKM.km1 (FA.q2_answerYes ^. uuid) `shouldBe` [FA.q2_aYes_fuQuestion1']
      describe "isThereAnyQuestionWithGivenUuid" $ do
        it "Returns True if exists" $
          isThereAnyQuestionWithGivenUuid FKM.km1 (FA.q2_aYes_fuQuestion1 ^. uuid) `shouldBe` True
        it "Returns False if not exists" $
          isThereAnyQuestionWithGivenUuid FKM.km1 (fromJust . U.fromString $ "c2dec208-3e58-473c-8cc3-a3964658e540") `shouldBe`
          False
    ---------------------------------------------
    describe "Expert" $ do
      describe "getAllExperts" $
        it "Successfully listed" $ getAllExperts FKM.km1 `shouldBe` [FE.expertAlbert, FE.expertNikola]
      describe "getExpertByUuid" $
        it "Successfully listed" $ getExpertByUuid FKM.km1 (FE.expertAlbert ^. uuid) `shouldBe` Just FE.expertAlbert
      describe "getAllExpertsForQuestionUuid" $
        it "Successfully listed" $
        getAllExpertsForQuestionUuid FKM.km1WithQ4 (FQ.q4_it1_question6 ^. uuid) `shouldBe`
        [FE.expertAlbert, FE.expertNikola]
      describe "isThereAnyExpertWithGivenUuid" $ do
        it "Returns True if exists" $ isThereAnyExpertWithGivenUuid FKM.km1 (FE.expertAlbert ^. uuid) `shouldBe` True
        it "Returns False if not exists" $
          isThereAnyExpertWithGivenUuid FKM.km1 (fromJust . U.fromString $ "c2dec208-3e58-473c-8cc3-a3964658e540") `shouldBe`
          False
    ---------------------------------------------
    describe "Reference" $ do
      describe "getAllReferences" $
        it "Successfully listed" $ getAllReferences FKM.km1 `shouldBe` [FR.referenceCh1', FR.referenceCh2']
      describe "getReferenceByUuid" $
        it "Successfully listed" $ getReferenceByUuid FKM.km1 (FR.referenceCh1 ^. uuid) `shouldBe` Just FR.referenceCh1'
      describe "getAllReferencesForQuestionUuid" $
        it "Successfully listed" $
        getAllReferencesForQuestionUuid FKM.km1WithQ4 (FQ.q4_it1_question6 ^. uuid) `shouldBe`
        [FR.referenceCh1', FR.referenceCh2']
      describe "isThereAnyReferenceWithGivenUuid" $ do
        it "Returns True if exists" $ isThereAnyReferenceWithGivenUuid FKM.km1 (FR.referenceCh1 ^. uuid) `shouldBe` True
        it "Returns False if not exists" $
          isThereAnyReferenceWithGivenUuid FKM.km1 (fromJust . U.fromString $ "c2dec208-3e58-473c-8cc3-a3964658e540") `shouldBe`
          False
    ---------------------------------------------
    describe "Answer" $ do
      describe "getAllAnswers" $
        it "Successfully listed" $
        getAllAnswers FKM.km1 `shouldBe`
        [ FA.q2_answerNo
        , FA.q2_answerYes
        , FA.q3_answerNo
        , FA.q3_answerYes
        , FA.q2_aYes_fuq1_answerNo
        , FA.q2_aYes_fuq1_answerYes
        , FA.q2_aYes_fuq1_aYes_fuq2_answerNo
        , FA.q2_aYes_fuq1_aYes_fuq2_answerYes
        ]
      describe "getAnswerByUuid" $
        it "Successfully listed" $
        getAnswerByUuid FKM.km1 (FA.q2_aYes_fuq1_answerNo ^. uuid) `shouldBe` Just FA.q2_aYes_fuq1_answerNo
      describe "getAllAnswersForQuestionUuid" $
        it "Successfully listed" $
        getAllAnswersForQuestionUuid FKM.km1 (FQ.question2 ^. uuid) `shouldBe` [FA.q2_answerNo, FA.q2_answerYes]
      describe "isThereAnyAnswerWithGivenUuid" $ do
        it "Returns True if exists" $
          isThereAnyAnswerWithGivenUuid FKM.km1 (FA.q2_aYes_fuq1_answerYes ^. uuid) `shouldBe` True
        it "Returns False if not exists" $
          isThereAnyAnswerWithGivenUuid FKM.km1 (fromJust . U.fromString $ "c2dec208-3e58-473c-8cc3-a3964658e540") `shouldBe`
          False
    ---------------------------------------------
    describe "Item Template Question" $ do
      describe "getAllItemTemplateQuestionsForQuestionUuid" $
        it "Successfully listed" $
        getAllItemTemplateQuestionsForQuestionUuid FKM.km1WithQ4 (FQ.q4_it1_question5 ^. uuid) `shouldBe`
        [FQ.q4_it1_q5_it2_question7', FQ.q4_it1_q5_it2_question8']
