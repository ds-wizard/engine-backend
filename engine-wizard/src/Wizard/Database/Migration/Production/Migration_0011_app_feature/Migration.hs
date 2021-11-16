module Wizard.Database.Migration.Production.Migration_0011_app_feature.Migration
  ( definition
  ) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Data.String (fromString)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 11, mmName = "Add app features", mmDescription = "Allow to turn app features"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  let sql = "ALTER TABLE app_config ADD feature json NOT NULL DEFAULT '{\"clientCustomizationEnabled\": false }';"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing
