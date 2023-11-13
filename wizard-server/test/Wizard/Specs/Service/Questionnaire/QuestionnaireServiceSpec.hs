module Wizard.Specs.Service.Questionnaire.QuestionnaireServiceSpec where

import Control.Monad.Reader (liftIO)
import Test.Hspec

import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import qualified Wizard.Database.Migration.Development.Package.PackageMigration as PKG_Migration
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireCommands
import qualified Wizard.Database.Migration.Development.Questionnaire.QuestionnaireMigration as QTN_Migration
import qualified Wizard.Database.Migration.Development.User.UserMigration as U_Migration
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Service.Questionnaire.QuestionnaireService
import WizardLib.Public.Model.PersistentCommand.Questionnaire.CreateQuestionnaireCommand

import Wizard.Specs.API.Common
import Wizard.Specs.Common

questionnaireServiceSpec appContext =
  describe "Questionnaire Service" $ do
    it "createQuestionnairesFromCommands" $
      -- GIVEN:
      do
        runInContextIO U_Migration.runMigration appContext
        runInContextIO PKG_Migration.runMigration appContext
        runInContextIO TML_Migration.runMigration appContext
        -- WHEN:
        (Right ()) <- runInContext (createQuestionnairesFromCommands [command1, command2]) appContext
        -- THEN:
        (Right questionnaires) <- runInContext findQuestionnaires appContext
        compareQuestionnaire (head questionnaires) command1
        compareQuestionnaire (questionnaires !! 1) command2

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

compareQuestionnaire :: Questionnaire -> CreateQuestionnaireCommand -> IO ()
compareQuestionnaire questionnaire command = liftIO $ do
  questionnaire.name `shouldBe` command.name
  questionnaire.packageId `shouldBe` command.packageId
  questionnaire.documentTemplateId `shouldBe` command.documentTemplateId
  length questionnaire.permissions `shouldBe` length command.emails
