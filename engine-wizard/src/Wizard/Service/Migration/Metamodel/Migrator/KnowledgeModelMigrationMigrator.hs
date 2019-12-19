module Wizard.Service.Migration.Metamodel.Migrator.KnowledgeModelMigrationMigrator
  ( migrate
  ) where

import Data.Aeson

import Shared.Model.Error.Error
import Wizard.Service.Migration.Metamodel.Migrator.Common

migrate :: Value -> Either AppError Value
migrate value =
  migrateEventsField "branchEvents" value >>= migrateEventsField "targetPackageEvents" >>=
  migrateEventsField "resultEvents" >>=
  migrateMetamodelVersionField
