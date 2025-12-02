module Wizard.Specs.API.Migration.KnowledgeModel.Common where

import Data.Either (isRight)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelEditorEventDAO
import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelMigrationDAO
import Wizard.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Wizard.Database.Migration.Development.KnowledgeModel.Data.Migration.KnowledgeModelMigrations
import qualified Wizard.Database.Migration.Development.KnowledgeModel.KnowledgeModelEditorMigration as KnowledgeModelEditor
import qualified Wizard.Database.Migration.Development.KnowledgeModel.KnowledgeModelMigrationMigration as KM_MIG
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorList
import Wizard.Model.KnowledgeModel.Migration.KnowledgeModelMigration
import Wizard.Service.KnowledgeModel.Migration.MigrationService

import Wizard.Specs.Common

-- --------------------------------
-- MIGRATION
-- --------------------------------
runMigrationWithEmptyDB appContext = do
  runInContextIO KnowledgeModelEditor.runMigration appContext
  runInContextIO (deleteKnowledgeModelEventsByEditorUuid amsterdamKnowledgeModelEditorList.uuid) appContext
  runInContextIO KM_MIG.runMigration appContext

runMigrationWithFullDB appContext = do
  runMigrationWithEmptyDB appContext
  runInContextIO (createMigration amsterdamKnowledgeModelEditorList.uuid migratorStateCreate) appContext

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertStateOfMigrationInDB appContext ms expState = do
  eMs <- runInContextIO (findMigratorStateByEditorUuid ms.editorUuid) appContext
  liftIO $ isRight eMs `shouldBe` True
  let (Right msFromDB) = eMs
  liftIO $ msFromDB.state `shouldBe` expState
