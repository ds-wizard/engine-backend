module Wizard.Service.Migration.Metamodel.Migrator.PackageMigrator (
  migrateAll,
) where

import Data.Aeson
import Data.Foldable (traverse_)
import Data.Time
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField

import Wizard.Model.Context.AppContext
import Wizard.Service.Migration.Metamodel.Migrator.CommonDB
import WizardLib.KnowledgeModel.Constant.KnowledgeModel

migrateAll :: AppContextM ()
migrateAll = do
  let (entityName, idField, eventsField, eventsFieldUpdate) = ("package", "id", "events", "events = ?")
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
  { entityId :: String
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
