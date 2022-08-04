module Wizard.Database.Migration.Production.Migration_0020_persistentCommandCreatedBy.Migration
  ( definition
  ) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Data.String (fromString)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta =
  MigrationMeta
    { mmNumber = 20
    , mmName = "Make createdBy in Persistent Command nullable"
    , mmDescription = "Remove not null constrain to createdBy in persistent_command"
    }

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  let sql =
        "ALTER TABLE persistent_command \
        \     ALTER COLUMN created_by DROP NOT NULL;"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing
