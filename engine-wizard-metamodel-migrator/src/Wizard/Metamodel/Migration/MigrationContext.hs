module Wizard.Metamodel.Migration.MigrationContext where

import Control.Lens (makeFields)
import Data.Time (UTCTime)

data MigrationContext =
  MigrationContext
    { _migrationContextCreatedAt :: UTCTime
    }
  deriving (Show, Eq)

makeFields ''MigrationContext
