module Service.Migration.Metamodel.Migrator.BranchMigrator
  ( migrate
  ) where

import Data.Aeson

import Model.Error.Error
import Service.Migration.Metamodel.Migrator.Common

migrate :: Value -> Either AppError Value
migrate value = migrateEventsField "events" value >>= migrateMetamodelVersionField
