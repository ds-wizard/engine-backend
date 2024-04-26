module Wizard.Database.Migration.Production.Migration_0044_signalBridge.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 44, mmName = "Signal Bridge", mmDescription = "Add signal bridge"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  let sql = "ALTER TABLE tenant ADD signal_bridge_url VARCHAR;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
