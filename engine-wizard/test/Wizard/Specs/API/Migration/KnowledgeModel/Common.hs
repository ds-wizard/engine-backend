module Wizard.Specs.API.Migration.KnowledgeModel.Common where

import Control.Lens ((^.))
import Data.Either (isRight)
import qualified Data.UUID as U
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import LensesConfig
import Wizard.Database.DAO.Branch.BranchDataDAO
import Wizard.Database.DAO.Migration.KnowledgeModel.MigratorDAO
import qualified Wizard.Database.Migration.Development.Branch.BranchMigration as B
import Wizard.Database.Migration.Development.Branch.Data.Branches
import Wizard.Database.Migration.Development.Migration.KnowledgeModel.Data.Migrations
import qualified Wizard.Database.Migration.Development.Migration.KnowledgeModel.MigratorMigration as KM_MIG
import Wizard.Service.Migration.KnowledgeModel.MigratorService

import Wizard.Specs.Common

-- --------------------------------
-- MIGRATION
-- --------------------------------
runMigrationWithEmptyDB appContext = do
  let branchUuid = U.toString $ amsterdamBranchDto ^. uuid
  runInContextIO B.runMigration appContext
  runInContextIO (updateBranchEventsByUuid branchUuid []) appContext
  runInContextIO KM_MIG.runMigration appContext

runMigrationWithFullDB appContext = do
  runMigrationWithEmptyDB appContext
  let branchUuid = U.toString $ amsterdamBranchDto ^. uuid
  runInContextIO (createMigration branchUuid migratorStateCreate) appContext

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertStateOfMigrationInDB appContext ms expState = do
  let bUuid = U.toString $ ms ^. branchUuid
  eMs <- runInContextIO (findMigratorStateByBranchUuid bUuid) appContext
  liftIO $ isRight eMs `shouldBe` True
  let (Right msFromDB) = eMs
  liftIO $ msFromDB ^. migrationState `shouldBe` expState
