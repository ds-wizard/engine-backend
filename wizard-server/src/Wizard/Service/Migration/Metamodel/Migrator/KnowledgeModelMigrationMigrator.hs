module Wizard.Service.Migration.Metamodel.Migrator.KnowledgeModelMigrationMigrator (
  migrateAll,
) where

import Data.Aeson
import Data.Foldable (traverse_)
import Data.Time
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField

import Wizard.Model.Context.AppContext
import Wizard.Service.Migration.Metamodel.Migrator.CommonDB
import WizardLib.KnowledgeModel.Constant.KnowledgeModel

migrateAll :: AppContextM ()
migrateAll = do
  let (entityName, idField, eventsField, eventsFieldUpdate) =
        ( "knowledge_model_migration"
        , "branch_uuid"
        , "branch_events, target_package_events, result_events"
        , "branch_events = ?, target_package_events = ?, result_events = ?"
        )
  logMigrationStarted entityName
  entities <- findOutdatedModels entityName eventsField idField
  traverse_ (migrateOneInDB entityName eventsFieldUpdate idField) entities
  logMigrationCompleted entityName

-- --------------------------------
-- PRIVATE
-- --------------------------------
migrateOneInDB :: String -> String -> String -> MetamodelMigration -> AppContextM ()
migrateOneInDB entityName eventsField idField MetamodelMigration {..} =
  migrateEventField entityName createdAt oldMetamodelVersion branchEvents $ \updatedBranchEvents ->
    migrateEventField entityName createdAt oldMetamodelVersion targetPackageEvents $ \updatedTargetPackageEvents ->
      migrateEventField entityName createdAt oldMetamodelVersion resultEvents $ \updatedResultEvents ->
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

data MetamodelMigration = MetamodelMigration
  { entityId :: U.UUID
  , createdAt :: UTCTime
  , oldMetamodelVersion :: Int
  , branchEvents :: Value
  , targetPackageEvents :: Value
  , resultEvents :: Value
  }

instance FromRow MetamodelMigration where
  fromRow = do
    entityId <- field
    createdAt <- field
    oldMetamodelVersion <- field
    branchEvents <- field
    targetPackageEvents <- field
    resultEvents <- field
    return $ MetamodelMigration {..}
