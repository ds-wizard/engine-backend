module Wizard.Service.Migration.Metamodel.Migrator.Common (
  convertValueToOject,
  getField,
  getArrayField,
  migrateMetamodelVersionField,
  migrateEventsField,
) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.String (fromString)
import qualified Data.Vector as Vector

import Shared.Constant.KnowledgeModel
import Shared.Model.Error.Error
import Shared.Util.JSON (convertValueToOject, getArrayField, getField)
import Shared.Util.List (foldEither)
import qualified Wizard.Metamodel.Migration.MigrationContext as EventMigrator
import qualified Wizard.Metamodel.Migrator.EventMigrator as EventMigrator

migrateMetamodelVersionField :: Value -> Either AppError Value
migrateMetamodelVersionField value =
  convertValueToOject value $ \object ->
    Right . Object $ KM.insert "metamodelVersion" (toJSON kmMetamodelVersion) object

migrateEventsField :: String -> Value -> Either AppError Value
migrateEventsField eventsFieldName value =
  convertValueToOject value $ \object ->
    getField "metamodelVersion" object $ \oldMetamodelVersion ->
      getField "createdAt" object $ \createdAt ->
        getArrayField eventsFieldName object $ \events ->
          case foldEither $
            EventMigrator.migrate (EventMigrator.MigrationContext createdAt) oldMetamodelVersion kmMetamodelVersion
              <$> Vector.toList events of
            Right updatedEvents ->
              Right . Object $ KM.insert (fromString eventsFieldName) (toJSON . concat $ updatedEvents) object
            Left error -> Left . GeneralServerError $ error
