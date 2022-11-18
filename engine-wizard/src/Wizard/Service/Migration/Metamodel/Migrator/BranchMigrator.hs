module Wizard.Service.Migration.Metamodel.Migrator.BranchMigrator (
  migrateAllInDB,
) where

import Data.Aeson
import Data.Foldable (traverse_)
import Data.Time
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField

import Shared.Constant.KnowledgeModel
import Wizard.Model.Context.AppContext
import Wizard.Service.Migration.Metamodel.Migrator.CommonDB

migrateAllInDB :: AppContextM ()
migrateAllInDB = do
  let (entityName, idField, eventsField, eventsFieldUpdate) = ("branch_data", "branch_uuid", "events", "events = ?")
  logMigrationStarted entityName
  entities <- findOutdatedModels entityName eventsField idField
  traverse_ (migrateOneInDB entityName eventsFieldUpdate idField) entities
  logMigrationCompleted entityName

-- --------------------------------
-- PRIVATE
-- --------------------------------
migrateOneInDB :: String -> String -> String -> MetamodelMigration -> AppContextM ()
migrateOneInDB entityName eventsField idField MetamodelMigration {..} =
  migrateEventField entityName createdAt oldMetamodelVersion events $ \updatedEvents ->
    updateOutdatedModels
      entityName
      eventsField
      idField
      [toField kmMetamodelVersion, toJSONField . toJSON $ updatedEvents, toField entityId]

data MetamodelMigration = MetamodelMigration
  { entityId :: U.UUID
  , createdAt :: UTCTime
  , oldMetamodelVersion :: Int
  , events :: Value
  }

instance FromRow MetamodelMigration where
  fromRow = do
    entityId <- field
    createdAt <- field
    oldMetamodelVersion <- field
    events <- field
    return $ MetamodelMigration {..}
