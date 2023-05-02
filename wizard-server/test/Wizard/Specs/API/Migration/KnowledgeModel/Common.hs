module Wizard.Specs.API.Migration.KnowledgeModel.Common where

import Data.Either (isRight)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Database.DAO.Branch.BranchDataDAO
import Wizard.Database.DAO.Migration.KnowledgeModel.MigratorDAO
import qualified Wizard.Database.Migration.Development.Branch.BranchMigration as B
import Wizard.Database.Migration.Development.Branch.Data.Branches
import Wizard.Database.Migration.Development.Migration.KnowledgeModel.Data.Migrations
import qualified Wizard.Database.Migration.Development.Migration.KnowledgeModel.MigratorMigration as KM_MIG
import Wizard.Model.Branch.BranchList
import Wizard.Model.Migration.KnowledgeModel.MigratorState
import Wizard.Service.Migration.KnowledgeModel.MigratorService

import Wizard.Specs.Common

-- --------------------------------
-- MIGRATION
-- --------------------------------
runMigrationWithEmptyDB appContext = do
  runInContextIO B.runMigration appContext
  runInContextIO (updateBranchEventsByUuid amsterdamBranchList.uuid []) appContext
  runInContextIO KM_MIG.runMigration appContext

runMigrationWithFullDB appContext = do
  runMigrationWithEmptyDB appContext
  runInContextIO (createMigration amsterdamBranchList.uuid migratorStateCreate) appContext

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertStateOfMigrationInDB appContext ms expState = do
  eMs <- runInContextIO (findMigratorStateByBranchUuid ms.branchUuid) appContext
  liftIO $ isRight eMs `shouldBe` True
  let (Right msFromDB) = eMs
  liftIO $ msFromDB.migrationState `shouldBe` expState
