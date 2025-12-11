module Wizard.Specs.Service.Project.ProjectServiceSpec where

import Control.Monad.Reader (liftIO)
import Test.Hspec

import Wizard.Database.DAO.Project.ProjectDAO
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import qualified Wizard.Database.Migration.Development.KnowledgeModel.KnowledgeModelPackageMigration as PKG_Migration
import Wizard.Database.Migration.Development.Project.Data.ProjectCommands
import qualified Wizard.Database.Migration.Development.Project.ProjectMigration as PRJ_Migration
import qualified Wizard.Database.Migration.Development.User.UserMigration as U_Migration
import Wizard.Model.Project.Project
import Wizard.Service.Project.ProjectService
import WizardLib.Public.Model.PersistentCommand.Project.CreateProjectCommand

import Wizard.Specs.API.Common
import Wizard.Specs.Common

projectServiceSpec appContext =
  describe "Project Service" $ do
    it "createProjectsFromCommands" $
      -- GIVEN:
      do
        runInContextIO U_Migration.runMigration appContext
        runInContextIO PKG_Migration.runMigration appContext
        runInContextIO TML_Migration.runMigration appContext
        -- WHEN:
        (Right ()) <- runInContext (createProjectsFromCommands [command1, command2]) appContext
        -- THEN:
        (Right projects) <- runInContext findProjects appContext
        length projects `shouldBe` 2
        compareProject (head projects) command1
        compareProject (projects !! 1) command2

    it "cleanProjects works" $
      -- GIVEN:
      do
        runInContextIO TML_Migration.runMigration appContext
        runInContextIO PRJ_Migration.runMigration appContext
        assertCountInDB findProjects appContext 3
        -- WHEN:
        (Right ()) <- runInContext cleanProjects appContext
        -- THEN:
        assertCountInDB findProjects appContext 2

compareProject :: Project -> CreateProjectCommand -> IO ()
compareProject project command = liftIO $ do
  project.name `shouldBe` command.name
  project.knowledgeModelPackageId `shouldBe` command.knowledgeModelPackageId
  project.documentTemplateId `shouldBe` command.documentTemplateId
  length project.permissions `shouldBe` length command.emails
