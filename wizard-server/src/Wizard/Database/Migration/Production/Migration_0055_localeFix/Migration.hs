module Wizard.Database.Migration.Production.Migration_0055_localeFix.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 55, mmName = "Fix missing default locale", mmDescription = "Fix misssing default locale"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  let sql =
        "UPDATE locale \
        \SET default_locale = true, \
        \    enabled        = true \
        \WHERE tenant_uuid IN (SELECT tenant_uuid \
        \                      FROM locale outer_locale \
        \                      WHERE (SELECT count(*) \
        \                            FROM locale inner_locale \
        \                            WHERE inner_locale.tenant_uuid = outer_locale.tenant_uuid) = 1 \
        \                        AND id = '~:default:1.0.0' \
        \                        AND (default_locale = false OR \
        \                            enabled = false))"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
