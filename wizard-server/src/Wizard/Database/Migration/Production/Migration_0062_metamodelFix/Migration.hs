module Wizard.Database.Migration.Production.Migration_0062_metamodelFix.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 62, mmName = "Fix metamodel version", mmDescription = "Fix metamodel version"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  let sql =
        "UPDATE knowledge_model_package \
        \SET metamodel_version = 19 \
        \WHERE tenant_uuid IN (SELECT uuid FROM tenant WHERE state = 'ReadyForUseTenantState'); \
        \ \
        \UPDATE knowledge_model_editor \
        \SET metamodel_version = 19 \
        \WHERE tenant_uuid IN (SELECT uuid FROM tenant WHERE state = 'ReadyForUseTenantState');"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
