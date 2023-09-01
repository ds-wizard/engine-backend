module Wizard.Service.Migration.Metamodel.Migrator.Migrations.MigrationContext where

import Data.Time (UTCTime)

data MigrationContext = MigrationContext
  { ctxCreatedAt :: UTCTime
  }
  deriving (Show, Eq)
