module Wizard.Specs.Service.KnowledgeModel.KnowledgeModelFilterSpec where

import Control.Lens ((&), (.~), (^.))
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import LensesConfig
import Shared.Database.Migration.Development.KnowledgeModel.Data.AnswersAndFollowUpQuestions
import Shared.Database.Migration.Development.KnowledgeModel.Data.Chapters
import Shared.Database.Migration.Development.KnowledgeModel.Data.Experts
import Shared.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Shared.Database.Migration.Development.KnowledgeModel.Data.Questions
import Shared.Database.Migration.Development.KnowledgeModel.Data.References
import Shared.Database.Migration.Development.KnowledgeModel.Data.Tags
import Shared.Model.KnowledgeModel.KnowledgeModelLenses
import Wizard.Service.KnowledgeModel.KnowledgeModelFilter

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
      let expectedKm =
            (chaptersL .~
             [ chapter1 & questionUuids .~ [question1 ^. uuid]
             , chapter2 & questionUuids .~ []
             , chapter3 & questionUuids .~ []
             ]) .
            (questionsL .~ [question1']) .
            (answersL .~ []) . (referencesL .~ []) . (expertsL .~ []) . (tagsL .~ [tagDataScience]) $
            km1WithQ4
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
      let expectedKm =
            (chaptersL .~ [chapter1, chapter2 & questionUuids .~ [question4 ^. uuid], chapter3]) .
            (questionsL .~
             [ question1'
             , question2'
             , q2_aYes_fuQuestion1'
             , question4' & itemTemplateQuestionUuids' .~ [q4_it1_question5Plain ^. uuid]
             , q4_it1_question5Plain'
             , question9'
             , question10'
             ]) .
            (answersL .~ [q2_answerNo, q2_answerYes, q2_aYes_fuq1_answerNo, q2_aYes_fuq1_answerYes]) .
            (referencesL .~ [km1_ch1_q2_r1', km1_ch1_q2_r2']) . (expertsL .~ [km1_ch1_q2_eAlbert, km1_ch1_q2_eNikola]) $
            km1WithQ4
        -- WHEN:
      let computedKm = filterKnowledgeModel inTags inKm
        -- THEN:
      computedKm `shouldBe` expectedKm
