module Wizard.Service.Migration.Metamodel.MigratorService where

import Control.Monad (void)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import Data.Aeson
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Util.JSON
import Shared.Common.Util.Uuid
import Shared.PersistentCommand.Database.DAO.PersistentCommand.PersistentCommandDAO
import Shared.PersistentCommand.Service.PersistentCommand.PersistentCommandMapper
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Tenant.TenantDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.PersistentCommand.Migration.Metamodel.MigrateToLatestMetamodelVersionCommand
import Wizard.Model.Tenant.Tenant
import qualified Wizard.Service.Migration.Metamodel.Migrator.BranchMigrator as BranchMigrator
import qualified Wizard.Service.Migration.Metamodel.Migrator.KnowledgeModelMigrationMigrator as KMMMigrator
import qualified Wizard.Service.Migration.Metamodel.Migrator.PackageBundleMigrator as PBMigrator
import qualified Wizard.Service.Migration.Metamodel.Migrator.PackageMigrator as PackageMigrator

migrateToLatestMetamodelVersionCommand :: Tenant -> Maybe U.UUID -> AppContextM ()
migrateToLatestMetamodelVersionCommand tenant mCreatedBy = do
  uuid <- liftIO generateUuid
  let dto = MigrateToLatestMetamodelVersionCommand {tenantUuid = tenant.uuid}
  let body = encodeJsonToString dto
  now <- liftIO getCurrentTime
  let command = toPersistentCommand uuid "metamodel_migrator" "migrate" body 10 True Nothing tenant.uuid (fmap U.toString mCreatedBy) now
  insertPersistentCommand command
  void $ updateTenantByUuid (tenant {state = HousekeepingInProgressTenantState, updatedAt = now})

migratePackageBundle :: Value -> AppContextM Value
migratePackageBundle value =
  runInTransaction $
    let eResult = PBMigrator.migrate value
     in case eResult of
          Right result -> return result
          Left error -> throwError error

migrateTenant :: AppContextM ()
migrateTenant =
  runInTransaction $ do
    PackageMigrator.migrateAll
    BranchMigrator.migrateAll
    KMMMigrator.migrateAll
    tenantUuid <- asks (.tenantUuid')
    now <- liftIO getCurrentTime
    tenant <- findTenantByUuid tenantUuid
    void $ updateTenantByUuid (tenant {state = ReadyForUseTenantState, updatedAt = now})
