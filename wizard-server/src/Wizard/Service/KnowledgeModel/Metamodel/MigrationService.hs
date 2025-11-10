module Wizard.Service.KnowledgeModel.Metamodel.MigrationService where

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
import qualified Wizard.Service.KnowledgeModel.Metamodel.Migrator.KnowledgeModelBundleMigrator as KnowledgeModelBundleMigrator
import qualified Wizard.Service.KnowledgeModel.Metamodel.Migrator.KnowledgeModelEditorMigrator as KnowledgeModelEditorMigrator
import qualified Wizard.Service.KnowledgeModel.Metamodel.Migrator.KnowledgeModelPackageMigrator as KnowledgeModelPackageMigrator

migrateToLatestMetamodelVersionCommand :: Tenant -> Maybe U.UUID -> AppContextM ()
migrateToLatestMetamodelVersionCommand tenant mCreatedBy = do
  uuid <- liftIO generateUuid
  let dto = MigrateToLatestMetamodelVersionCommand {tenantUuid = tenant.uuid}
  let body = encodeJsonToString dto
  now <- liftIO getCurrentTime
  let command = toPersistentCommand uuid "metamodel_migrator" "migrate" body 10 True Nothing tenant.uuid (fmap U.toString mCreatedBy) now
  insertPersistentCommand command
  void $ updateTenantByUuid (tenant {state = HousekeepingInProgressTenantState, updatedAt = now})

migrateKnowledgeModelBundle :: Value -> AppContextM Value
migrateKnowledgeModelBundle value =
  runInTransaction $
    let eResult = KnowledgeModelBundleMigrator.migrate value
     in case eResult of
          Right result -> return result
          Left error -> throwError error

migrateTenant :: AppContextM ()
migrateTenant =
  runInTransaction $ do
    KnowledgeModelPackageMigrator.migrateAll
    KnowledgeModelEditorMigrator.migrateAll
    tenantUuid <- asks (.tenantUuid')
    now <- liftIO getCurrentTime
    tenant <- findTenantByUuid tenantUuid
    void $ updateTenantByUuid (tenant {state = ReadyForUseTenantState, updatedAt = now})
