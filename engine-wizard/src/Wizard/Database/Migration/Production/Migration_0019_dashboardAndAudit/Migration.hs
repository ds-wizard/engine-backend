module Wizard.Database.Migration.Production.Migration_0019_dashboardAndAudit.Migration
  ( definition
  ) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Data.String (fromString)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta =
  MigrationMeta
    {mmNumber = 19, mmName = "Dashboard and Audit", mmDescription = "Rename app config dashboard and add audit"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  addDashboardTypeField dbPool
  removeWidgetField dbPool
  createAuditTable dbPool

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

createAuditTable dbPool = do
  let sql =
        "CREATE TABLE audit \
        \     ( \
        \         uuid            uuid not null \
        \             constraint audit_pk \
        \                 primary key, \
        \         component       varchar not null, \
        \         action          varchar not null, \
        \         entity          varchar not null, \
        \         body            json not null, \
        \         created_by      uuid, \
        \         app_uuid uuid default '00000000-0000-0000-0000-000000000000' not null \
        \           constraint audit_app_uuid_fk \
        \             references app, \
        \         created_at      timestamp with time zone not null \
        \     ); \
        \CREATE UNIQUE INDEX audit_uuid_uindex \
        \     ON audit (uuid); \
        \  \
        \ALTER TABLE audit \
        \    ADD CONSTRAINT audit_user_entity_uuid_fk \
        \       FOREIGN KEY (created_by) REFERENCES user_entity (uuid) ON DELETE CASCADE;"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing
