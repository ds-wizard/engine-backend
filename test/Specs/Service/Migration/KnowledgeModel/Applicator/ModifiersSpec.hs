module Specs.Service.Migration.KnowledgeModel.Applicator.ModifiersSpec where

import Control.Lens
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Database.Migration.Development.Event.Data.Events
import Database.Migration.Development.KnowledgeModel.Data.AnswersAndFollowUpQuestions
import Database.Migration.Development.KnowledgeModel.Data.Chapters
import Database.Migration.Development.KnowledgeModel.Data.Experts
import Database.Migration.Development.KnowledgeModel.Data.Integrations
import Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Database.Migration.Development.KnowledgeModel.Data.Questions
import Database.Migration.Development.KnowledgeModel.Data.References
import Database.Migration.Development.KnowledgeModel.Data.Tags
import LensesConfig
import Model.KnowledgeModel.KnowledgeModel
import Service.Migration.KnowledgeModel.Applicator.Modifiers

modifiersSpec =
  describe "Modifier" $ do
    describe "Knowledge Model level" $ do
      describe "createKM" $
        it "Successfully created" $
          -- GIVEN: Inputs
         do
          let event = a_km1
          -- AND: Expectations
          let expected = km1WithoutChaptersAndTagsAndIntegrations
          -- WHEN:
          let computed = createKM event
          -- THEN:
          computed `shouldBe` expected
      describe "editKM" $
        it "Successfully edited" $
          -- GIVEN: Inputs
         do
          let event = e_km1
          -- AND: Expectations
          let expected = km1Edited
          -- WHEN:
          let computed = editKM event km1
          -- THEN:
          computed `shouldBe` expected
      describe "addChapter" $
        it "Successfully added" $
          -- GIVEN: Inputs
         do
          let chapter = chapter1
          -- AND: Expectations
          let expected = km1WithoutChaptersAndTagsAndIntegrations & chapters .~ [chapter1]
          -- WHEN:
          let computed = addChapter km1WithoutChaptersAndTagsAndIntegrations chapter
          -- THEN:
          computed `shouldBe` expected
      describe "deleteChapter" $
        it "Successfully deleted" $
          -- GIVEN: Inputs
         do
          let chapterUuid = chapter2 ^. uuid
          -- AND: Expectations
          let expected = km1 & chapters .~ [chapter1, chapter3]
          -- WHEN:
          let computed = deleteChapter km1 chapterUuid
          -- THEN:
          computed `shouldBe` expected
      describe "addTag" $
        it "Successfully added" $
          -- GIVEN: Inputs
         do
          let tag = tagDataScience
          -- AND: Expectations
          let expected = km1WithoutChaptersAndTagsAndIntegrations & tags .~ [tagDataScience]
          -- WHEN:
          let computed = addTag km1WithoutChaptersAndTagsAndIntegrations tag
          -- THEN:
          computed `shouldBe` expected
      describe "deleteTag" $
        it "Successfully deleted" $
          -- GIVEN: Inputs
         do
          let tagUuid = tagBioInformatic ^. uuid
          -- AND: Expectations
          let expected = km1 & tags .~ [tagDataScience]
          -- WHEN:
          let computed = deleteTag km1 tagUuid
          -- THEN:
          computed `shouldBe` expected
      describe "addIntegration" $
        it "Successfully added" $
          -- GIVEN: Inputs
         do
          let integration = ontologyPortal
          -- AND: Expectations
          let expected = km1WithoutChaptersAndTagsAndIntegrations & integrations .~ [ontologyPortal]
          -- WHEN:
          let computed = addIntegration km1WithoutChaptersAndTagsAndIntegrations integration
          -- THEN:
          computed `shouldBe` expected
      describe "deleteIntegration" $
        it "Successfully deleted" $
          -- GIVEN: Inputs
         do
          let integrationUuid = ontologyPortal ^. uuid
          -- AND: Expectations
          let expected = km1 & integrations .~ [bioPortal]
          -- WHEN:
          let computed = deleteIntegration km1 integrationUuid
          -- THEN:
          computed `shouldBe` expected
    describe "Chapter level" $ do
      describe "createChapter" $
        it "Successfully created" $
          -- GIVEN: Inputs
         do
          let event = a_km1_ch1
          -- AND: Expectations
          let expected = chapter1WithoutQuestions
          -- WHEN:
          let computed = createChapter event
          -- THEN:
          computed `shouldBe` expected
      describe "editChapter" $
        it "Successfully edited" $
          -- GIVEN: Inputs
         do
          let event = e_km1_ch1
          -- AND: Expectations
          let expected = chapter1Edited
          -- WHEN:
          let computed = editChapter event chapter1
          -- THEN:
          computed `shouldBe` expected
      describe "addQuestion" $
        it "Successfully added" $
          -- GIVEN: Inputs
         do
          let question = question1'
          -- AND: Expectations
          let expected = chapter1WithoutQuestions & questions .~ [question1']
          -- WHEN:
          let computed = addQuestion chapter1WithoutQuestions question
          -- THEN:
          computed `shouldBe` expected
      describe "deleteQuestion" $
        it "Successfully deleted" $
          -- GIVEN: Inputs
         do
          let questionUuid = question2 ^. uuid
          -- AND: Expectations
          let expected = chapter1 & questions .~ [question1']
          -- WHEN:
          let computed = deleteQuestion chapter1 questionUuid
          -- THEN:
          computed `shouldBe` expected
    describe "Question level" $ do
      describe "createQuestion" $ do
        describe "OptionsQuestion" $
          it "Successfully created" $
            -- GIVEN: Inputs
           do
            let event = a_km1_ch1_q2'
            -- AND: Expectations
            let expected = question2Plain'
            -- WHEN:
            let computed = createQuestion event
            -- THEN:
            computed `shouldBe` expected
        describe "ListQuestion" $
          it "Successfully created" $
            -- GIVEN: Inputs
           do
            let event = a_km1_ch2_q4'
            -- AND: Expectations
            let expected = question4Plain'
            -- WHEN:
            let computed = createQuestion event
            -- THEN:
            computed `shouldBe` expected
        describe "ValueQuestion" $
          it "Successfully created" $
            -- GIVEN: Inputs
           do
            let event = a_km1_ch1_q1'
            -- AND: Expectations
            let expected = question1'
            -- WHEN:
            let computed = createQuestion event
            -- THEN:
            computed `shouldBe` expected
      describe "editQuestion" $ do
        describe "OptionsQuestion" $ do
          describe "Without changing question type" $
            it "Successfully created" $
              -- GIVEN: Inputs
             do
              let event = e_km1_ch1_q2'
              -- AND: Expectations
              let expected = question2Edited'
              -- WHEN:
              let computed = editQuestion event question2'
              -- THEN:
              computed `shouldBe` expected
          describe "With changing question type" $
            it "Successfully created" $
              -- GIVEN: Inputs
             do
              let event = e_km1_ch1_q2_type'
              -- AND: Expectations
              let expected = question2WithNewType'
              -- WHEN:
              let computed = editQuestion event question2'
              -- THEN:
              computed `shouldBe` expected
        describe "ListQuestion" $ do
          describe "Without changing question type" $
            it "Successfully created" $
              -- GIVEN: Inputs
             do
              let event = e_km1_ch2_q4'
              -- AND: Expectations
              let expected = question4Edited'
              -- WHEN:
              let computed = editQuestion event question4'
              -- THEN:
              computed `shouldBe` expected
          describe "With changing question type" $
            it "Successfully created" $
              -- GIVEN: Inputs
             do
              let event = e_km1_ch2_q4_type'
              -- AND: Expectations
              let expected = question4WithNewType'
              -- WHEN:
              let computed = editQuestion event question4'
              -- THEN:
              computed `shouldBe` expected
        describe "ValueQuestion" $ do
          describe "Without changing question type" $
            it "Successfully created" $
              -- GIVEN: Inputs
             do
              let event = e_km1_ch1_q1'
              -- AND: Expectations
              let expected = question1Edited'
              -- WHEN:
              let computed = editQuestion event question1'
              -- THEN:
              computed `shouldBe` expected
          describe "With changing question type" $
            it "Successfully created" $
              -- GIVEN: Inputs
             do
              let event = e_km1_ch1_q1_type'
              -- AND: Expectations
              let expected = question1WithNewType'
              -- WHEN:
              let computed = editQuestion event question1'
              -- THEN:
              computed `shouldBe` expected
        describe "IntegrationQuestion" $ do
          describe "Without changing question type" $
            it "Successfully created" $
              -- GIVEN: Inputs
             do
              let event = e_km1_ch3_q9'
              -- AND: Expectations
              let expected = question9Edited'
              -- WHEN:
              let computed = editQuestion event question9'
              -- THEN:
              computed `shouldBe` expected
          describe "With changing question type" $
            it "Successfully created" $
              -- GIVEN: Inputs
             do
              let event = e_km1_ch3_q9_type'
              -- AND: Expectations
              let expected = question9WithNewType'
              -- WHEN:
              let computed = editQuestion event question9'
              -- THEN:
              computed `shouldBe` expected
      describe "updateIntegrationProps" $ do
        describe "OptionsQuestion" $ do
          it "Do nothing with the question" $
            -- GIVEN: Inputs
           do
            let event = e_km1_iop
            -- AND: Expectations
            let expected = question2'
            -- WHEN:
            let computed = updateIntegrationProps event question2'
            -- THEN:
            computed `shouldBe` expected
        describe "ListQuestion" $ do
          it "Do nothing with the question" $
            -- GIVEN: Inputs
           do
            let event = e_km1_iop
            -- AND: Expectations
            let expected = question4'
            -- WHEN:
            let computed = updateIntegrationProps event question4'
            -- THEN:
            computed `shouldBe` expected
        describe "ValueQuestion" $ do
          it "Do nothing with the question" $
            -- GIVEN: Inputs
           do
            let event = e_km1_iop
            -- AND: Expectations
            let expected = question1'
            -- WHEN:
            let computed = updateIntegrationProps event question1'
            -- THEN:
            computed `shouldBe` expected
        describe "IntegrationQuestion" $ do
          it "Update the props" $
            -- GIVEN: Inputs
           do
            let event = e_km1_iop
            -- AND: Expectations
            let expected = question9PropsEdited'
            -- WHEN:
            let computed = updateIntegrationProps event question9'
            -- THEN:
            computed `shouldBe` expected
      describe "addItemTemplateQuestion" $
        it "Successfully added" $
        -- GIVEN: Inputs
         do
          let question = q4_it1_question5'
        -- AND: Expectations
          let expected = ListQuestion' $ question4 & itemTemplateQuestions .~ [q4_it1_question5']
        -- WHEN:
          let computed = addItemTemplateQuestion question4Plain' question
        -- THEN:
          computed `shouldBe` expected
      describe "deleteItemTemplateQuestion" $
        it "Successfully deleted" $
        -- GIVEN: Inputs
         do
          let questionUuid = q4_it1_question5 ^. uuid
        -- AND: Expectations
          let expected = ListQuestion' $ question4 & itemTemplateQuestions .~ [q4_it1_question6']
        -- WHEN:
          let computed = deleteItemTemplateQuestion question4' questionUuid
        -- THEN:
          computed `shouldBe` expected
      describe "addAnswer" $
        it "Successfully added" $
        -- GIVEN: Inputs
         do
          let answer = q2_answerNo
        -- AND: Expectations
          let expected = OptionsQuestion' $ question2Plain & answers .~ [q2_answerNo]
        -- WHEN:
          let computed = addAnswer question2Plain' answer
        -- THEN:
          computed `shouldBe` expected
      describe "deleteAnswer" $
        it "Successfully deleted" $
        -- GIVEN: Inputs
         do
          let answerUuid = q2_answerNo ^. uuid
        -- AND: Expectations
          let expected = OptionsQuestion' $ question2 & answers .~ [q2_answerYes]
        -- WHEN:
          let computed = deleteAnswer question2' answerUuid
        -- THEN:
          computed `shouldBe` expected
      describe "addExpert" $
        it "Successfully added" $
        -- GIVEN: Inputs
         do
          let expert = expertAlbert
        -- AND: Expectations
          let expected = OptionsQuestion' $ question2Plain & experts .~ [expertAlbert]
        -- WHEN:
          let computed = addExpert question2Plain' expert
        -- THEN:
          computed `shouldBe` expected
      describe "deleteExpert" $
        it "Successfully deleted" $
        -- GIVEN: Inputs
         do
          let expertUuid = expertAlbert ^. uuid
        -- AND: Expectations
          let expected = OptionsQuestion' $ question2 & experts .~ [expertNikola]
        -- WHEN:
          let computed = deleteExpert question2' expertUuid
        -- THEN:
          computed `shouldBe` expected
      describe "addReference" $
        it "Successfully added" $
        -- GIVEN: Inputs
         do
          let reference = referenceCh1'
        -- AND: Expectations
          let expected = OptionsQuestion' $ question2Plain & references .~ [referenceCh1']
        -- WHEN:
          let computed = addReference question2Plain' reference
        -- THEN:
          computed `shouldBe` expected
      describe "deleteReference" $
        it "Successfully deleted" $
        -- GIVEN: Inputs
         do
          let referenceUuid = referenceCh1 ^. uuid
        -- AND: Expectations
          let expected = OptionsQuestion' $ question2 & references .~ [referenceCh2']
        -- WHEN:
          let computed = deleteReference question2' referenceUuid
        -- THEN:
          computed `shouldBe` expected
    describe "Answer level" $ do
      describe "createAnswer" $
        it "Successfully created" $
          -- GIVEN: Inputs
         do
          let event = a_km1_ch1_q2_aNo1
          -- AND: Expectations
          let expected = q2_answerNo
          -- WHEN:
          let computed = createAnswer event
          -- THEN:
          computed `shouldBe` expected
      describe "editAnswer" $
        it "Successfully edited" $
          -- GIVEN: Inputs
         do
          let event = e_km1_ch1_q2_aYes1
          -- AND: Expectations
          let expected = q2_answerYesEdited
          -- WHEN:
          let computed = editAnswer event q2_answerYes
          -- THEN:
          computed `shouldBe` expected
      describe "addFuQuestion" $
        it "Successfully added" $
          -- GIVEN: Inputs
         do
          let question = q2_aYes_fuQuestion1'
          -- AND: Expectations
          let expected = q2_answerYes
          -- WHEN:
          let computed = addFuQuestion q2_answerYesPlain question
          -- THEN:
          computed `shouldBe` expected
      describe "deleteFuQuestion" $
        it "Successfully deleted" $
          -- GIVEN: Inputs
         do
          let questionUuid = q2_aYes_fuQuestion1 ^. uuid
          -- AND: Expectations
          let expected = q2_answerYesPlain
          -- WHEN:
          let computed = deleteFuQuestion q2_answerYes questionUuid
          -- THEN:
          computed `shouldBe` expected
    describe "Expert level" $ do
      describe "createExpert" $
        it "Successfully created" $
          -- GIVEN: Inputs
         do
          let event = a_km1_ch1_q2_eAlbert
          -- AND: Expectations
          let expected = expertAlbert
          -- WHEN:
          let computed = createExpert event
          -- THEN:
          computed `shouldBe` expected
      describe "editExpert" $
        it "Successfully edited" $
          -- GIVEN: Inputs
         do
          let event = e_km1_ch1_q2_eAlbert
          -- AND: Expectations
          let expected = expertAlbertEdited
          -- WHEN:
          let computed = editExpert event expertAlbert
          -- THEN:
          computed `shouldBe` expected
    describe "Reference level" $ do
      describe "createReference" $ do
        describe "ResourcePageReference" $
          it "Successfully created" $
            -- GIVEN: Inputs
           do
            let event = a_km1_ch1_q2_rCh1'
            -- AND: Expectations
            let expected = referenceCh1'
            -- WHEN:
            let computed = createReference event
            -- THEN:
            computed `shouldBe` expected
        describe "URLReference" $
          it "Successfully created" $
            -- GIVEN: Inputs
           do
            let event = a_km1_ch1_q2_rCh2'
            -- AND: Expectations
            let expected = referenceCh2'
            -- WHEN:
            let computed = createReference event
            -- THEN:
            computed `shouldBe` expected
        describe "CrossReference" $
          it "Successfully created" $
            -- GIVEN: Inputs
           do
            let event = a_km1_ch1_q2_rCh3'
            -- AND: Expectations
            let expected = referenceCh3'
            -- WHEN:
            let computed = createReference event
            -- THEN:
            computed `shouldBe` expected
      describe "editReference" $ do
        describe "ResourcePageReference" $ do
          describe "Without changing question type" $
            it "Successfully created" $
              -- GIVEN: Inputs
             do
              let event = e_km1_ch1_q2_rCh1'
              -- AND: Expectations
              let expected = referenceCh1Edited'
              -- WHEN:
              let computed = editReference event referenceCh1'
              -- THEN:
              computed `shouldBe` expected
          describe "With changing question type" $
            it "Successfully created" $
              -- GIVEN: Inputs
             do
              let event = e_km1_ch1_q2_rCh1_type'
              -- AND: Expectations
              let expected = referenceCh1WithNewType'
              -- WHEN:
              let computed = editReference event referenceCh1'
              -- THEN:
              computed `shouldBe` expected
        describe "URLReference" $ do
          describe "Without changing question type" $
            it "Successfully created" $
              -- GIVEN: Inputs
             do
              let event = e_km1_ch1_q2_rCh2'
              -- AND: Expectations
              let expected = referenceCh2Edited'
              -- WHEN:
              let computed = editReference event referenceCh2'
              -- THEN:
              computed `shouldBe` expected
          describe "With changing question type" $
            it "Successfully created" $
              -- GIVEN: Inputs
             do
              let event = e_km1_ch1_q2_rCh2_type'
              -- AND: Expectations
              let expected = referenceCh2WithNewType'
              -- WHEN:
              let computed = editReference event referenceCh2'
              -- THEN:
              computed `shouldBe` expected
        describe "CrossReference" $ do
          describe "Without changing question type" $
            it "Successfully created" $
              -- GIVEN: Inputs
             do
              let event = e_km1_ch1_q2_rCh3'
              -- AND: Expectations
              let expected = referenceCh3Edited'
              -- WHEN:
              let computed = editReference event referenceCh3'
              -- THEN:
              computed `shouldBe` expected
          describe "With changing question type" $
            it "Successfully created" $
              -- GIVEN: Inputs
             do
              let event = e_km1_ch1_q2_rCh3_type'
              -- AND: Expectations
              let expected = referenceCh3WithNewType'
              -- WHEN:
              let computed = editReference event referenceCh3'
              -- THEN:
              computed `shouldBe` expected
    describe "Tag level" $ do
      describe "createTag" $
        it "Successfully created" $
          -- GIVEN: Inputs
         do
          let event = a_km1_tds
          -- AND: Expectations
          let expected = tagDataScience
          -- WHEN:
          let computed = createTag event
          -- THEN:
          computed `shouldBe` expected
      describe "editTag" $
        it "Successfully edited" $
          -- GIVEN: Inputs
         do
          let event = e_km1_tds
          -- AND: Expectations
          let expected = tagDataScienceEdited
          -- WHEN:
          let computed = editTag event tagDataScience
          -- THEN:
          computed `shouldBe` expected
    describe "Integration level" $ do
      describe "createIntegration" $
        it "Successfully created" $
          -- GIVEN: Inputs
         do
          let event = a_km1_iop
          -- AND: Expectations
          let expected = ontologyPortal
          -- WHEN:
          let computed = createIntegration event
          -- THEN:
          computed `shouldBe` expected
      describe "editIntegration" $
        it "Successfully edited" $
          -- GIVEN: Inputs
         do
          let event = e_km1_iop
          -- AND: Expectations
          let expected = ontologyPortalEdited
          -- WHEN:
          let computed = editIntegration event ontologyPortal
          -- THEN:
          computed `shouldBe` expected
