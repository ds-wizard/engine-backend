module Wizard.Service.Migration.Metamodel.Migrator.Common
  ( convertValueToOject
  , getField
  , getArrayField
  , migrateMetamodelVersionField
  , migrateEventsField
  ) where

import Data.Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T
import qualified Data.Vector as Vector

import Shared.Constant.KnowledgeModel
import Shared.Model.Error.Error
import Shared.Util.JSON (convertValueToOject, getArrayField, getField)
import qualified Wizard.Metamodel.Migrator.EventMigrator as EventMigrator
import Wizard.Util.List (foldEither)

migrateMetamodelVersionField :: Value -> Either AppError Value
migrateMetamodelVersionField value =
  convertValueToOject value $ \object ->
    Right . Object $ HashMap.insert "metamodelVersion" (toJSON kmMetamodelVersion) object

migrateEventsField :: String -> Value -> Either AppError Value
migrateEventsField eventsFieldName value =
  convertValueToOject value $ \object ->
    getField "metamodelVersion" object $ \oldMetamodelVersion ->
      getArrayField eventsFieldName object $ \events ->
        case foldEither $ EventMigrator.migrate oldMetamodelVersion kmMetamodelVersion <$> Vector.toList events of
          Right updatedEvents -> Right . Object $ HashMap.insert (T.pack eventsFieldName) (toJSON updatedEvents) object
          Left error -> Left . GeneralServerError $ error
