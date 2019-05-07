module Specs.Service.KnowledgeModel.KnowledgeModelFilterSpec where

import Control.Lens ((&), (.~), (^.))
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Database.Migration.Development.KnowledgeModel.Data.AnswersAndFollowUpQuestions
import Database.Migration.Development.KnowledgeModel.Data.Chapters
import Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Database.Migration.Development.KnowledgeModel.Data.Questions
import Database.Migration.Development.KnowledgeModel.Data.Tags
import LensesConfig
import Model.KnowledgeModel.KnowledgeModel
import Service.KnowledgeModel.KnowledgeModelFilter

knowledgeModelFilterSpec =
  describe "KnowledgeModelFilter" $
  describe "filterKnowledgeModel" $ do
    it "No tags provided" $
        -- GIVEN: Prepare inputs
     do
      let inTags = []
      let inKm = km1WithQ4
        -- AND: Prepare expectations
      let expectedKm = km1WithQ4
        -- WHEN:
      let computedKm = filterKnowledgeModel inTags inKm
        -- THEN:
      computedKm `shouldBe` expectedKm
    it "1 tag provided" $
        -- GIVEN: Prepare inputs
     do
      let inTags = [tagDataScience ^. uuid]
      let inKm = km1WithQ4
        -- AND: Prepare expectations
      let expChapter1 = chapter1 & questions .~ [question1']
      let expectedKm = (km1WithQ4 & chapters .~ [expChapter1]) & tags .~ [tagDataScience]
        -- WHEN:
      let computedKm = filterKnowledgeModel inTags inKm
        -- THEN:
      computedKm `shouldBe` expectedKm
    it "2 tags provided" $
        -- GIVEN: Prepare inputs
     do
      let inTags = [tagDataScience ^. uuid, tagBioInformatic ^. uuid]
      let inKm = km1WithQ4
        -- AND: Prepare expectations
        -- Chapter 1
      let expQ2_aYes_fuq1_answerYes = q2_aYes_fuq1_answerYes & followUps .~ []
      let expQ2_aYes_fuQuestion1 = q2_aYes_fuQuestion1 & answers .~ [q2_aYes_fuq1_answerNo, expQ2_aYes_fuq1_answerYes]
      let expQ2_answerYes = q2_answerYes & followUps .~ [OptionsQuestion' expQ2_aYes_fuQuestion1]
      let expQuestion2 = question2 & answers .~ [q2_answerNo, expQ2_answerYes]
      let expChapter1 = chapter1 & questions .~ [question1', OptionsQuestion' expQuestion2]
        -- Chapter 2
      let expQ4_it1_question5 = q4_it1_question5 & itemTemplateQuestions .~ []
      let expQuestion4 = question4 & itemTemplateQuestions .~ [ListQuestion' expQ4_it1_question5]
      let expChapter2 = chapter2 & questions .~ [ListQuestion' expQuestion4]
        -- Knowledge Model
      let expectedKm = km1WithQ4 & chapters .~ [expChapter1, expChapter2, chapter3]
        -- WHEN:
      let computedKm = filterKnowledgeModel inTags inKm
        -- THEN:
      computedKm `shouldBe` expectedKm
