module Wizard.Service.Migration.Metamodel.Migrator.KnowledgeModelMigrationMigrator
  ( migrateAllInDB
  ) where

import Data.Aeson
import Data.Foldable (traverse_)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField

import Shared.Constant.KnowledgeModel
import Wizard.Model.Context.AppContext
import Wizard.Service.Migration.Metamodel.Migrator.CommonDB

migrateAllInDB :: AppContextM ()
migrateAllInDB = do
  let (entityName, idField, eventsField, eventsFieldUpdate) =
        ( "km_migration"
        , "branch_uuid"
        , "branch_events, target_package_events, result_events"
        , "branch_events = ?, target_package_events = ?, result_events = ?")
  logMigrationStarted entityName
  entities <- findOutdatedModels entityName eventsField idField
  traverse_ (migrateOneInDB entityName eventsFieldUpdate idField) entities
  logMigrationCompleted entityName

-- --------------------------------
-- PRIVATE
-- --------------------------------
migrateOneInDB :: String -> String -> String -> MetamodelMigration -> AppContextM ()
migrateOneInDB entityName eventsField idField MetamodelMigration {..} =
  migrateEventField entityName oldMetamodelVersion branchEvents $ \updatedBranchEvents ->
    migrateEventField entityName oldMetamodelVersion targetPackageEvents $ \updatedTargetPackageEvents ->
      migrateEventField entityName oldMetamodelVersion resultEvents $ \updatedResultEvents ->
        updateOutdatedModels
          entityName
          eventsField
          idField
          [ toField kmMetamodelVersion
          , toJSONField . toJSON $ updatedBranchEvents
          , toJSONField . toJSON $ updatedTargetPackageEvents
          , toJSONField . toJSON $ updatedResultEvents
          , toField entityId
          ]

data MetamodelMigration =
  MetamodelMigration
    { entityId :: U.UUID
    , oldMetamodelVersion :: Int
    , branchEvents :: Value
    , targetPackageEvents :: Value
    , resultEvents :: Value
    }

instance FromRow MetamodelMigration where
  fromRow = do
    entityId <- field
    oldMetamodelVersion <- field
    branchEvents <- field
    targetPackageEvents <- field
    resultEvents <- field
    return $ MetamodelMigration {..}
