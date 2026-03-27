module Wizard.Database.Migration.Production.Migration_0066_tcKnowledgeModel.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 66, mmName = "Remove TC knowledge model", mmDescription = "Remove the TC knowledge model table"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  dropConfigKnowledgeModelTable dbPool
  dropValueIdColumns dbPool

dropConfigKnowledgeModelTable dbPool = do
  let sql = "DROP TABLE config_knowledge_model"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

dropValueIdColumns dbPool = do
  let sql =
        "ALTER TABLE knowledge_model_editor_reply DROP COLUMN value_id; \
        \ALTER TABLE project_event DROP COLUMN value_id;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
