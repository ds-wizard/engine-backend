module Wizard.Database.Migration.Production.Migration_0030_twoFactorAuth.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 30, mmName = "Add 2FA", mmDescription = "Add 2 factor authentication"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  let sql =
        "UPDATE app_config \
        \SET authentication = \
        \        jsonb_set( \
        \                to_jsonb(authentication), \
        \                '{internal}', \
        \                concat('{\"registration\":{\"enabled\":', authentication -> 'internal' -> 'registration' -> 'enabled', '}, \"twoFactorAuth\": { \"enabled\": false, \"codeLength\": 6, \"expiration\": 600 }}')::jsonb, \
        \                false \
        \            )"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
