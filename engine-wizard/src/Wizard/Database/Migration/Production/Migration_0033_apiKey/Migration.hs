module Wizard.Database.Migration.Production.Migration_0033_apiKey.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 33, mmName = "API Key", mmDescription = "Add API Key"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  clearUserTokenTable dbPool
  extendUserTokenTable dbPool

clearUserTokenTable dbPool = do
  let sql = "DELETE FROM user_token WHERE uuid is not null;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

extendUserTokenTable dbPool = do
  let sql =
        "ALTER TABLE user_token \
        \   ADD name varchar NOT NULL,\
        \   ADD type varchar NOT NULL,\
        \   ADD user_agent varchar NOT NULL,\
        \   ADD expires_at timestamp with time zone NOT NULL;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
