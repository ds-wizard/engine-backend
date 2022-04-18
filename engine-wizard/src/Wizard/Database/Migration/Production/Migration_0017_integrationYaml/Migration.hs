module Wizard.Database.Migration.Production.Migration_0017_integrationYaml.Migration
  ( definition
  ) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Data.String (fromString)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 17, mmName = "Integration Yaml", mmDescription = "Add integration yaml to DB"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  let sql =
        "UPDATE app_config \
        \SET knowledge_model = knowledge_model::jsonb || '{\"integrationConfig\": \"\"}'::jsonb \
        \WHERE uuid IS NOT NULL"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing
