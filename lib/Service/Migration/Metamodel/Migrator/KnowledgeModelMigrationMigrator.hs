module Service.Migration.Metamodel.Migrator.KnowledgeModelMigrationMigrator
  ( migrate
  ) where

import Data.Aeson

import Model.Error.Error
import Service.Migration.Metamodel.Migrator.Common

migrate :: Value -> Either AppError Value
migrate value =
  migrateMetamodelVersionField value >>= migrateEventsField "branchEvents" >>= migrateEventsField "targetPackageEvents" >>=
  migrateEventsField "resultEvents"
