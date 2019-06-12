module Service.Migration.Metamodel.Migrator.PackageBundleMigrator
  ( migrate
  ) where

import Data.Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as Vector

import Model.Error.Error
import Service.Migration.Metamodel.Migrator.Common
import qualified
       Service.Migration.Metamodel.Migrator.PackageMigrator
       as PackageMigrator
import Util.JSON (convertValueToOject, getArrayField)
import Util.List (foldEither)

migrate :: Value -> Either AppError Value
migrate value = migratePackagesField value >>= migrateMetamodelVersionField

-- --------------------------------
-- PRIVATE
-- --------------------------------
migratePackagesField :: Value -> Either AppError Value
migratePackagesField value =
  convertValueToOject value $ \object ->
    getArrayField "packages" object $ \packages -> do
      case foldEither $ PackageMigrator.migrate <$> (Vector.toList packages) of
        Right updatedPackages -> Right . Object $ HashMap.insert "packages" (toJSON updatedPackages) object
        Left error -> Left error
