module Wizard.Service.Migration.Metamodel.Migrator.Common (
  convertValueToObject,
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
import Shared.Common.Util.JSON (convertValueToObject, getArrayField, getField)
import Shared.Common.Util.List (foldEither)
import Wizard.Localization.Messages.Public
import qualified Wizard.Service.Migration.Metamodel.Migrator.EventMigrator as EventMigrator
import qualified Wizard.Service.Migration.Metamodel.Migrator.Migrations.MigrationContext as EventMigrator
import WizardLib.KnowledgeModel.Constant.KnowledgeModel

validateMetamodelVersionField :: Value -> Either AppError Value
validateMetamodelVersionField value =
  convertValueToObject value $ \object ->
    getField "metamodelVersion" object $ \metamodelVersion ->
      if metamodelVersion <= kmMetamodelVersion
        then Right value
        else Left . UserError $ _ERROR_VALIDATION__PKG_UNSUPPORTED_METAMODEL_VERSION metamodelVersion kmMetamodelVersion

migrateMetamodelVersionField :: Value -> Either AppError Value
migrateMetamodelVersionField value =
  convertValueToObject value $ \object ->
    Right . Object $ KM.insert "metamodelVersion" (toJSON kmMetamodelVersion) object

migrateEventsField :: String -> Value -> Either AppError Value
migrateEventsField eventsFieldName value =
  convertValueToObject value $ \object ->
    getField "metamodelVersion" object $ \oldMetamodelVersion ->
      getField "createdAt" object $ \createdAt ->
        getArrayField eventsFieldName object $ \events ->
          case foldEither $
            EventMigrator.migrate (EventMigrator.MigrationContext createdAt) oldMetamodelVersion kmMetamodelVersion
              <$> Vector.toList events of
            Right updatedEvents ->
              Right . Object $ KM.insert (fromString eventsFieldName) (toJSON . concat $ updatedEvents) object
            Left error -> Left . GeneralServerError $ error
