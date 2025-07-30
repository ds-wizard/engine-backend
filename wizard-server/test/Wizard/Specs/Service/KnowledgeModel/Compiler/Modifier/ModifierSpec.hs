module Wizard.Specs.Service.KnowledgeModel.Compiler.Modifier.ModifierSpec where

import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Wizard.Service.KnowledgeModel.Compiler.Modifier.Answer ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Chapter ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Expert ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Integration ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.KnowledgeModel ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Metric ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Modifier
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Phase ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Question
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Reference ()
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Tag ()
import WizardLib.KnowledgeModel.Database.Migration.Development.Event.Data.Events
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.AnswersAndFollowUpQuestions
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Chapters
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Experts
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Integrations
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Metrics
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Phases
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Questions
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.References
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Tags

modifierSpec =
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
            let computed = createEntity event
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
            let computed = editEntity event km1
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
            let computed = createEntity event
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
            let computed = editEntity event chapter1
            -- THEN:
            computed `shouldBe` expected
    describe "Question level" $ do
      describe "createEntity" $ do
        describe "OptionsQuestion" $
          it "Successfully created" $
            -- GIVEN: Inputs
            do
              let event = a_km1_ch1_q2'
              -- AND: Expectations
              let expected = question2Plain'
              -- WHEN:
              let computed = createEntity event
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
              let computed = createEntity event
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
              let computed = createEntity event
              -- THEN:
              computed `shouldBe` expected
      describe "editEntity" $ do
        describe "OptionsQuestion" $ do
          describe "Without changing question type" $
            it "Successfully created" $
              -- GIVEN: Inputs
              do
                let event = e_km1_ch1_q2'
                -- AND: Expectations
                let expected = question2Edited'
                -- WHEN:
                let computed = editEntity event question2'
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
                let computed = editEntity event question2'
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
                let computed = editEntity event question4'
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
                let computed = editEntity event question4'
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
                let computed = editEntity event question1'
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
                let computed = editEntity event question1'
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
                let computed = editEntity event question9'
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
                let computed = editEntity event question9'
                -- THEN:
                computed `shouldBe` expected
      describe "updateIntegrationVariables" $ do
        describe "OptionsQuestion" $
          it "Do nothing with the question" $
            -- GIVEN: Inputs
            do
              let event = e_km1_iop'
              -- AND: Expectations
              let expected = question2'
              -- WHEN:
              let computed = updateIntegrationVariables event question2'
              -- THEN:
              computed `shouldBe` expected
        describe "ListQuestion" $
          it "Do nothing with the question" $
            -- GIVEN: Inputs
            do
              let event = e_km1_iop'
              -- AND: Expectations
              let expected = question4'
              -- WHEN:
              let computed = updateIntegrationVariables event question4'
              -- THEN:
              computed `shouldBe` expected
        describe "ValueQuestion" $
          it "Do nothing with the question" $
            -- GIVEN: Inputs
            do
              let event = e_km1_iop'
              -- AND: Expectations
              let expected = question1'
              -- WHEN:
              let computed = updateIntegrationVariables event question1'
              -- THEN:
              computed `shouldBe` expected
        describe "IntegrationQuestion" $
          it "Update the variables" $
            -- GIVEN: Inputs
            do
              let event = e_km1_iop'
              -- AND: Expectations
              let expected = question9VariablesEdited'
              -- WHEN:
              let computed = updateIntegrationVariables event question9'
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
            let computed = createEntity event
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
            let computed = editEntity event q2_answerYes
            -- THEN:
            computed `shouldBe` expected
    describe "Expert level" $ do
      describe "createExpert" $
        it "Successfully created" $
          -- GIVEN: Inputs
          do
            let event = a_km1_ch1_q2_eAlbert
            -- AND: Expectations
            let expected = km1_ch1_q2_eAlbert
            -- WHEN:
            let computed = createEntity event
            -- THEN:
            computed `shouldBe` expected
      describe "editExpert" $
        it "Successfully edited" $
          -- GIVEN: Inputs
          do
            let event = e_km1_ch1_q2_eAlbert
            -- AND: Expectations
            let expected = km1_ch1_q2_eAlbertEdited
            -- WHEN:
            let computed = editEntity event km1_ch1_q2_eAlbert
            -- THEN:
            computed `shouldBe` expected
    describe "Reference level" $ do
      describe "createEntity" $ do
        describe "ResourcePageReference" $
          it "Successfully created" $
            -- GIVEN: Inputs
            do
              let event = a_km1_ch1_q2_rCh1'
              -- AND: Expectations
              let expected = km1_ch1_q2_r1'
              -- WHEN:
              let computed = createEntity event
              -- THEN:
              computed `shouldBe` expected
        describe "URLReference" $
          it "Successfully created" $
            -- GIVEN: Inputs
            do
              let event = a_km1_ch1_q2_rCh2'
              -- AND: Expectations
              let expected = km1_ch1_q2_r2'
              -- WHEN:
              let computed = createEntity event
              -- THEN:
              computed `shouldBe` expected
        describe "CrossReference" $
          it "Successfully created" $
            -- GIVEN: Inputs
            do
              let event = a_km1_ch1_q2_rCh3'
              -- AND: Expectations
              let expected = km1_ch1_q2_r3'
              -- WHEN:
              let computed = createEntity event
              -- THEN:
              computed `shouldBe` expected
      describe "editEntity" $ do
        describe "ResourcePageReference" $ do
          describe "Without changing question type" $
            it "Successfully created" $
              -- GIVEN: Inputs
              do
                let event = e_km1_ch1_q2_rCh1'
                -- AND: Expectations
                let expected = km1_ch1_q2_r1Edited'
                -- WHEN:
                let computed = editEntity event km1_ch1_q2_r1'
                -- THEN:
                computed `shouldBe` expected
          describe "With changing question type" $
            it "Successfully created" $
              -- GIVEN: Inputs
              do
                let event = e_km1_ch1_q2_rCh1_type'
                -- AND: Expectations
                let expected = km1_ch1_q2_r1WithNewType'
                -- WHEN:
                let computed = editEntity event km1_ch1_q2_r1'
                -- THEN:
                computed `shouldBe` expected
        describe "URLReference" $ do
          describe "Without changing question type" $
            it "Successfully created" $
              -- GIVEN: Inputs
              do
                let event = e_km1_ch1_q2_rCh2'
                -- AND: Expectations
                let expected = km1_ch1_q2_r2Edited'
                -- WHEN:
                let computed = editEntity event km1_ch1_q2_r2'
                -- THEN:
                computed `shouldBe` expected
          describe "With changing question type" $
            it "Successfully created" $
              -- GIVEN: Inputs
              do
                let event = e_km1_ch1_q2_rCh2_type'
                -- AND: Expectations
                let expected = km1_ch1_q2_r2WithNewType'
                -- WHEN:
                let computed = editEntity event km1_ch1_q2_r2'
                -- THEN:
                computed `shouldBe` expected
        describe "CrossReference" $ do
          describe "Without changing question type" $
            it "Successfully created" $
              -- GIVEN: Inputs
              do
                let event = e_km1_ch1_q2_rCh3'
                -- AND: Expectations
                let expected = km1_ch1_q2_r3Edited'
                -- WHEN:
                let computed = editEntity event km1_ch1_q2_r3'
                -- THEN:
                computed `shouldBe` expected
          describe "With changing question type" $
            it "Successfully created" $
              -- GIVEN: Inputs
              do
                let event = e_km1_ch1_q2_rCh3_type'
                -- AND: Expectations
                let expected = km1_ch1_q2_r3WithNewType'
                -- WHEN:
                let computed = editEntity event km1_ch1_q2_r3'
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
            let computed = createEntity event
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
            let computed = editEntity event tagDataScience
            -- THEN:
            computed `shouldBe` expected
    describe "Integration level" $ do
      describe "createIntegration" $ do
        describe "ApiLegacyIntegration" $
          it "Successfully created" $
            -- GIVEN: Inputs
            do
              let event = a_km1_iop'
              -- AND: Expectations
              let expected = ontologyPortal'
              -- WHEN:
              let computed = createEntity event
              -- THEN:
              computed `shouldBe` expected
        describe "WidgetIntegration" $
          it "Successfully created" $
            -- GIVEN: Inputs
            do
              let event = a_km1_iwp'
              -- AND: Expectations
              let expected = widgetPortal'
              -- WHEN:
              let computed = createEntity event
              -- THEN:
              computed `shouldBe` expected
      describe "editIntegration" $ do
        describe "ApiLegacyIntegration" $
          it "Successfully edited" $
            -- GIVEN: Inputs
            do
              let event = e_km1_iop'
              -- AND: Expectations
              let expected = ontologyPortalEdited'
              -- WHEN:
              let computed = editEntity event ontologyPortal'
              -- THEN:
              computed `shouldBe` expected
        describe "WidgetIntegration" $
          it "Successfully edited" $
            -- GIVEN: Inputs
            do
              let event = e_km1_iwp'
              -- AND: Expectations
              let expected = widgetPortalEdited'
              -- WHEN:
              let computed = editEntity event widgetPortal'
              -- THEN:
              computed `shouldBe` expected
    describe "Metric level" $ do
      describe "createMetric" $
        it "Successfully created" $
          -- GIVEN: Inputs
          do
            let event = a_km1_mtrF
            -- AND: Expectations
            let expected = metricF
            -- WHEN:
            let computed = createEntity event
            -- THEN:
            computed `shouldBe` expected
      describe "editMetric" $
        it "Successfully edited" $
          -- GIVEN: Inputs
          do
            let event = e_km1_mtrF
            -- AND: Expectations
            let expected = metricFEdited
            -- WHEN:
            let computed = editEntity event metricF
            -- THEN:
            computed `shouldBe` expected
    describe "Phase level" $ do
      describe "createPhase" $
        it "Successfully created" $
          -- GIVEN: Inputs
          do
            let event = a_km1_phs1
            -- AND: Expectations
            let expected = phase1
            -- WHEN:
            let computed = createEntity event
            -- THEN:
            computed `shouldBe` expected
      describe "editPhase" $
        it "Successfully edited" $
          -- GIVEN: Inputs
          do
            let event = e_km1_phs1
            -- AND: Expectations
            let expected = phase1Edited
            -- WHEN:
            let computed = editEntity event phase1
            -- THEN:
            computed `shouldBe` expected
