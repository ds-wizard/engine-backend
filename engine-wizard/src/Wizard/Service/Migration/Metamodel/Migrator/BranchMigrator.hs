module Wizard.Service.Migration.Metamodel.Migrator.BranchMigrator
  ( migrate
  ) where

import Data.Aeson

import Shared.Model.Error.Error
import Wizard.Service.Migration.Metamodel.Migrator.Common

migrate :: Value -> Either AppError Value
migrate value = migrateEventsField "events" value >>= migrateMetamodelVersionField
