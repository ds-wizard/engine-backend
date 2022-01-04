module Wizard.Database.Migration.Production.Migration_0012_projectTagging.Migration
  ( definition
  ) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Data.String (fromString)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 12, mmName = "Project Tagging", mmDescription = "Add possibility to tag projects"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  updateAppConfig dbPool
  addProjectTagsToQuestionnaire dbPool

-- ------------------------------------------------------------------------------------------------------------
updateAppConfig dbPool = do
  let sql =
        "UPDATE app_config \
        \SET questionnaire=jsonb_set(to_jsonb(questionnaire), '{projectTagging}', '{\"enabled\": true, \"tags\": []}') \
        \WHERE uuid IS NOT NULL"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing

-- ------------------------------------------------------------------------------------------------------------
addProjectTagsToQuestionnaire dbPool = do
  let sql =
        "ALTER TABLE questionnaire ADD project_tags TEXT[] NOT NULL DEFAULT '{}'; \
            \ALTER TABLE questionnaire RENAME COLUMN selected_tag_uuids TO selected_question_tag_uuids;"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing
