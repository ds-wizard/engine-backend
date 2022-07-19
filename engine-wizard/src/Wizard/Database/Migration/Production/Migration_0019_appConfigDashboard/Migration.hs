module Wizard.Database.Migration.Production.Migration_0019_appConfigDashboard.Migration
  ( definition
  ) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Data.String (fromString)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 19, mmName = "App Config Dashboard", mmDescription = "Rename app config dashboard"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  addDashboardTypeField dbPool
  removeWidgetField dbPool

addDashboardTypeField dbPool = do
  let sql =
        "UPDATE app_config \
        \SET dashboard = dashboard::jsonb || jsonb_build_object('dashboardType', 'RoleBasedDashboardType') \
        \WHERE uuid is not null;"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing

removeWidgetField dbPool = do
  let sql =
        "UPDATE app_config \
        \SET dashboard = dashboard::jsonb - 'widgets' \
        \WHERE uuid is not null;"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing
