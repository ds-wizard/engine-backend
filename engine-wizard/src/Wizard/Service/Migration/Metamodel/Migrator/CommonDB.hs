module Wizard.Service.Migration.Metamodel.Migrator.CommonDB where

import Data.Aeson
import Data.String (fromString)
import qualified Data.Vector as Vector
import Database.PostgreSQL.Simple

import Shared.Constant.Component
import Shared.Constant.KnowledgeModel
import Shared.Localization.Messages.Public
import Shared.Util.List (foldEither)
import Wizard.Database.DAO.Common
import Wizard.Localization.Messages.Internal
import qualified Wizard.Metamodel.Migration.MigrationContext as EventMigrator
import qualified Wizard.Metamodel.Migrator.EventMigrator as EventMigrator
import Wizard.Model.Context.ContextLenses ()
import Wizard.Util.Logger

-- --------------------------------
-- COMMON
-- --------------------------------
findOutdatedModels entityName eventsField idField = do
  let sql =
        f'
          "SELECT %s, created_at, metamodel_version, %s FROM %s WHERE metamodel_version != ?"
          [idField, eventsField, entityName]
  logInfo _CMP_DATABASE sql
  let action conn = query conn (fromString sql) [kmMetamodelVersion]
  runDB action

updateOutdatedModels entityName eventsField idField params = do
  let sql = f' "UPDATE %s SET metamodel_version = ?, %s WHERE %s = ?" [entityName, eventsField, idField]
  logInfo _CMP_DATABASE sql
  let action conn = execute conn (fromString sql) params
  runDB action
  return ()

migrateEventField entityName createdAt oldMetamodelVersion events callback =
  case events of
    Array eventArray ->
      case foldEither $
        EventMigrator.migrate (EventMigrator.MigrationContext createdAt) oldMetamodelVersion kmMetamodelVersion
          <$> Vector.toList eventArray of
        Right updatedEvents -> do
          logMigrationMigrationApplied entityName
          callback . concat $ updatedEvents
        Left error -> logMigrationFailedToConvertToNewMetamodelVersion entityName error
    _ -> logMigrationFailedToMigrateCollection entityName (_ERROR_UTIL_JSON__BAD_FIELD_TYPE "events" "Array")

-- --------------------------------
-- LOGGER
-- --------------------------------
logMigrationStarted entityName = logInfo _CMP_SERVICE $ "Metamodel Migration ('" ++ entityName ++ "'): started"

logMigrationMigrationApplied entityName =
  logInfo _CMP_SERVICE $ "Metamodel Migration ('" ++ entityName ++ "'): migration applied"

logMigrationCompleted entityName = logInfo _CMP_SERVICE $ "Metamodel Migration ('" ++ entityName ++ "'): completed"

logMigrationFailedToConvertToNewMetamodelVersion entityName error = do
  logError _CMP_SERVICE $ _ERROR_SERVICE_MIGRATION_METAMODEL__FAILED_CONVERT_TO_NEW_METAMODEL entityName
  logError _CMP_SERVICE . show $ error
  return ()

logMigrationFailedToMigrateCollection entityName error = do
  logError _CMP_SERVICE $ _ERROR_SERVICE_MIGRATION_METAMODEL__FAILED_TO_MIGRATE_ENTITIES entityName
  logError _CMP_SERVICE . show $ error
  return ()
