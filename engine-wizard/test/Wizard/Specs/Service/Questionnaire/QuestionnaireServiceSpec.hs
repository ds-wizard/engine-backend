module Wizard.Specs.Service.Questionnaire.QuestionnaireServiceSpec where

import Test.Hspec

import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import qualified Wizard.Database.Migration.Development.Questionnaire.QuestionnaireMigration as QTN_Migration
import qualified Wizard.Database.Migration.Development.Template.TemplateMigration as TML_Migration
import Wizard.Service.Questionnaire.QuestionnaireService

import Wizard.Specs.API.Common
import Wizard.Specs.Common

questionnaireServiceSpec appContext =
  describe "Questionnaire Service" $
    it "cleanQuestionnaires works" $
      -- GIVEN:
      do
        runInContextIO TML_Migration.runMigration appContext
        runInContextIO QTN_Migration.runMigration appContext
        assertCountInDB findQuestionnaires appContext 3
        -- WHEN:
        (Right ()) <- runInContext cleanQuestionnaires appContext
        -- THEN:
        assertCountInDB findQuestionnaires appContext 2
