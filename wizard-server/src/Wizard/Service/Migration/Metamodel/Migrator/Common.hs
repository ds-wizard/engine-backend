module Wizard.Service.Migration.Metamodel.Migrator.Common (
  convertValueToOject,
  getField,
  getArrayField,
  migrateMetamodelVersionField,
  migrateEventsField,
  validateMetamodelVersionField,
) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.String (fromString)
import qualified Data.Vector as Vector

import Shared.Common.Model.Error.Error
import Shared.Common.Util.JSON (convertValueToOject, getArrayField, getField)
import Shared.Common.Util.List (foldEither)
import Wizard.Localization.Messages.Public
import WizardLib.KnowledgeModel.Constant.KnowledgeModel
import qualified WizardLib.Metamodel.Migration.MigrationContext as EventMigrator
import qualified WizardLib.Metamodel.Migrator.EventMigrator as EventMigrator

validateMetamodelVersionField :: Value -> Either AppError Value
validateMetamodelVersionField value =
  convertValueToOject value $ \object ->
    getField "metamodelVersion" object $ \metamodelVersion ->
      if metamodelVersion <= kmMetamodelVersion
        then Right value
        else Left . UserError $ _ERROR_VALIDATION__PKG_UNSUPPORTED_METAMODEL_VERSION metamodelVersion kmMetamodelVersion

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
