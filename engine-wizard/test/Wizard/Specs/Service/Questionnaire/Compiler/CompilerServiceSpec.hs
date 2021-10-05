module Wizard.Specs.Service.Questionnaire.Compiler.CompilerServiceSpec where

import Control.Lens ((&), (.~), (^.))
import qualified Data.Map.Strict as M
import Test.Hspec

import LensesConfig
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireComments
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireEvents
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireLabels
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireReplies
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Service.Questionnaire.Compiler.CompilerService

import Wizard.Specs.Common

questionnaireCompilerServiceSpec appContext =
  describe "Questionnaire Compiler Service" $
  describe "applyEvent" $ do
    it "SetReplyEvent" $
        -- GIVEN:
     do
      let event = sre_rQ1Updated'
        -- WHEN:
      (Right updatedQtn) <- runInContext (applyEvent (return questionnaire1Ctn) event) appContext
        -- THEN:
      updatedQtn ^. replies `shouldBe` fRepliesWithUpdated
    it "ClearReplyEvent" $
        -- GIVEN:
     do
      let event = cre_rQ1'
        -- WHEN:
      (Right updatedQtn) <- runInContext (applyEvent (return questionnaire1Ctn) event) appContext
        -- THEN:
      updatedQtn ^. replies `shouldBe` fRepliesWithDeleted
    it "SetPhaseEvent" $
        -- GIVEN:
     do
      let event = sphse_2'
        -- WHEN:
      (Right updatedQtn) <- runInContext (applyEvent (return questionnaire1Ctn) event) appContext
        -- THEN:
      updatedQtn ^. phaseUuid `shouldBe` sphse_2 ^. phaseUuid
    it "SetLabelsEvent" $
        -- GIVEN:
     do
      let event = slble_rQ2'
        -- WHEN:
      (Right updatedQtn) <- runInContext (applyEvent (return questionnaire1Ctn) event) appContext
        -- THEN:
      updatedQtn ^. labels `shouldBe` fLabelsEdited
    it "ResolveCommentThreadEvent" $
        -- GIVEN:
     do
      let event = rte_rQ1_t1'
        -- WHEN:
      (Right updatedQtn) <- runInContext (applyEvent (return questionnaire1Ctn) event) appContext
        -- THEN:
      updatedQtn ^. commentThreadsMap `shouldBe` M.insert cmtQ1_path [cmtQ1_t1Resolved] qtnThreads
    it "ReopenCommentThreadEvent" $
        -- GIVEN:
     do
      let event = ote_rQ1_t1'
      let questionnaireQtn = questionnaire1Ctn & commentThreadsMap .~ M.insert cmtQ1_path [cmtQ1_t1Resolved] qtnThreads
        -- WHEN:
      (Right updatedQtn) <- runInContext (applyEvent (return questionnaireQtn) event) appContext
        -- THEN:
      updatedQtn ^. commentThreadsMap `shouldBe` questionnaire1Ctn ^. commentThreadsMap
    it "DeleteCommentThreadEvent" $
        -- GIVEN:
     do
      let event = dte_rQ1_t1'
        -- WHEN:
      (Right updatedQtn) <- runInContext (applyEvent (return questionnaire1Ctn) event) appContext
        -- THEN:
      updatedQtn ^. commentThreadsMap `shouldBe` M.insert cmtQ1_path [] qtnThreads
    it "AddCommentEvent" $
        -- GIVEN:
     do
      let event = ace_rQ2_t1_1'
        -- WHEN:
      (Right updatedQtn) <- runInContext (applyEvent (return questionnaire1Ctn) event) appContext
        -- THEN:
      updatedQtn ^. commentThreadsMap `shouldBe` M.insert cmtQ2_path [cmtQ2_t1] qtnThreads
    it "EditCommentEvent" $
        -- GIVEN:
     do
      let event = ece_rQ1_t1_1'
        -- WHEN:
      (Right updatedQtn) <- runInContext (applyEvent (return questionnaire1Ctn) event) appContext
        -- THEN:
      updatedQtn ^. commentThreadsMap `shouldBe` M.insert cmtQ1_path [cmtQ1_t1WithEditedCmt] qtnThreads
    it "DeleteCommentEvent" $
        -- GIVEN:
     do
      let event = dce_rQ1_t1_1'
        -- WHEN:
      (Right updatedQtn) <- runInContext (applyEvent (return questionnaire1Ctn) event) appContext
        -- THEN:
      updatedQtn ^. commentThreadsMap `shouldBe` M.insert cmtQ1_path [cmtQ1_t1WithDeletedCmt] qtnThreads
