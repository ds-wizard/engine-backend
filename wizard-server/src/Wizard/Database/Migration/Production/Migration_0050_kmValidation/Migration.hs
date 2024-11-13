module Wizard.Database.Migration.Production.Migration_0050_kmValidation.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 50, mmName = "Add KM validation", mmDescription = "Add validation for questions in Knowledge Model"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  let sql =
        "DELETE FROM knowledge_model_cache; \
        \UPDATE tenant SET state = 'PendingHousekeepingTenantState';"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
