module Wizard.Service.KnowledgeModel.Metamodel.Migrator.KnowledgeModelBundleMigrator (
  migrate,
) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.String (fromString)
import qualified Data.Vector as Vector

import Shared.Common.Model.Error.Error
import Shared.Common.Util.List (foldEither)
import Wizard.Service.KnowledgeModel.Metamodel.Migrator.Common

migrate :: Value -> Either AppError Value
migrate value = migratePackagesField value >>= migrateMetamodelVersionField

-- --------------------------------
-- PRIVATE
-- --------------------------------
migratePackagesField :: Value -> Either AppError Value
migratePackagesField value =
  convertValueToObject value $ \object ->
    getArrayField "packages" object $ \packages ->
      case foldEither $ migratePackage <$> Vector.toList packages of
        Right updatedPackages -> Right . Object $ KM.insert (fromString "packages") (toJSON updatedPackages) object
        Left error -> Left error

migratePackage :: Value -> Either AppError Value
migratePackage value = validateMetamodelVersionField value >>= migrateEventsField "events" >>= migrateMetamodelVersionField
