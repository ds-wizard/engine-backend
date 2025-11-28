module Wizard.Database.Migration.Production.Migration_0061_news.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 61, mmName = "Add user news", mmDescription = "Add user news feature"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  let sql = "ALTER TABLE user_entity ADD COLUMN last_seen_news_id varchar;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
