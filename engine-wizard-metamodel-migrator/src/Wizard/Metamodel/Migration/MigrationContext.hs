module Wizard.Metamodel.Migration.MigrationContext where

import Data.Time (UTCTime)

data MigrationContext = MigrationContext
  { ctxCreatedAt :: UTCTime
  }
  deriving (Show, Eq)
