module Wizard.Specs.Service.KnowledgeModel.KnowledgeModelFilterSpec where

import qualified Data.Map.Strict as M
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Wizard.Service.KnowledgeModel.KnowledgeModelFilter
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.AnswersAndFollowUpQuestions
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Chapters
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Experts
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Questions
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.References
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Tags
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModelLenses

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
          let inTags = [tagDataScience.uuid]
          let inKm = km1WithQ4
          -- AND: Prepare expectations
          let expectedKm =
                km1WithQ4
                  { entities =
                      km1WithQ4.entities
                        { chapters = toMap [chapter1 {questionUuids = [question1.uuid]}, chapter2 {questionUuids = []}, chapter3 {questionUuids = []}]
                        , questions = toMap [setTagUuids question1' [tagDataScience.uuid]]
                        , answers = M.empty
                        , references = M.empty
                        , experts = M.empty
                        , tags = toMap [tagDataScience]
                        }
                  , tagUuids = inTags
                  }
          -- WHEN:
          let computedKm = filterKnowledgeModel inTags inKm
          -- THEN:
          computedKm `shouldBe` expectedKm
      it "2 tags provided" $
        -- GIVEN: Prepare inputs
        do
          let inTags = [tagDataScience.uuid, tagBioInformatic.uuid]
          let inKm = km1WithQ4
          -- AND: Prepare expectations
          let expectedKm =
                km1WithQ4
                  { entities =
                      km1WithQ4.entities
                        { chapters = toMap [chapter1, chapter2 {questionUuids = [question4.uuid]}, chapter3]
                        , questions =
                            toMap
                              [ question1'
                              , question2'
                              , q2_aYes_fuQuestion1'
                              , setItemTemplateQuestionUuids question4' [q4_it1_question5Plain.uuid]
                              , q4_it1_question5Plain'
                              , question9'
                              , question10'
                              , question11'
                              , question12'
                              , question13'
                              , question14'
                              , question15'
                              ]
                        , answers = toMap [q2_answerNo, q2_answerYes, q2_aYes_fuq1_answerNo, q2_aYes_fuq1_answerYes]
                        , references = toMap [km1_ch1_q2_r1', km1_ch1_q2_r2']
                        , experts = toMap [km1_ch1_q2_eAlbert, km1_ch1_q2_eNikola]
                        }
                  }
          -- WHEN:
          let computedKm = filterKnowledgeModel inTags inKm
          -- THEN:
          computedKm `shouldBe` expectedKm
