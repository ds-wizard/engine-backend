module Wizard.Database.Migration.Production.Migration_0046_tenantFeature.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 46, mmName = "Tenant Feature", mmDescription = "Remove tenant feature column"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  let sql = "ALTER TABLE tenant_config DROP feature"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
