module Wizard.Database.Migration.Production.Migration_0016_appPlan.Migration
  ( definition
  ) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Data.String (fromString)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 16, mmName = "App Plan", mmDescription = "Adjust app plan"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  let sql =
        "ALTER TABLE app_plan \
        \   ALTER COLUMN since DROP NOT NULL, \
        \   ALTER COLUMN until DROP NOT NULL; "
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing
