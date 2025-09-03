module Wizard.Database.Migration.Production.Migration_0059_integrationReplyFix.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 59, mmName = "Fix integration reply", mmDescription = "Fix integration reply in branch data"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  let sql = "UPDATE branch_data SET replies = (replace(replies::text, 'IntegrationType', 'IntegrationLegacyType'))::jsonb;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
