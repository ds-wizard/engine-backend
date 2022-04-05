module Wizard.Database.Migration.Production.Migration_0015_prefab.Migration
  ( definition
  ) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Data.String (fromString)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 15, mmName = "Prefab", mmDescription = "Add prefabs, app plan, and owl"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  createPrefabTable dbPool
  addOwlToAppConfig dbPool
  createAppPlanTable dbPool

createPrefabTable dbPool = do
  let sql =
        "CREATE TABLE prefab \
        \ ( \
        \     uuid              uuid              not null, \
        \     type              varchar           not null,\
        \     name              varchar           not null,\
        \     content           json              not null, \
        \     app_uuid uuid default '00000000-0000-0000-0000-000000000000' not null \
        \       constraint prefab_app_uuid_fk \
        \         references app, \
        \     created_at timestamp with time zone not null,\
        \     updated_at timestamp with time zone not null, \
        \     constraint prefab_pk \
        \        primary key (uuid, app_uuid) \
        \ ); \
        \  \
        \CREATE UNIQUE INDEX prefab_uuid_uindex \
        \     on prefab (uuid, app_uuid);"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing

addOwlToAppConfig dbPool = do
  let sql =
        "ALTER TABLE app_config \
        \   ADD owl json not null default '{\"enabled\":false,\"kmId\":\"\",\"name\":\"\",\"version\":\"\",\"rootElement\":\"\",\"organizationId\":\"\",\"previousPackageId\":null}';"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing

createAppPlanTable dbPool = do
  let sql =
        "CREATE TABLE app_plan \
         \ ( \
         \     uuid              uuid              not null \
         \         constraint app_plan_pk \
         \             primary key, \
         \     name              varchar not null, \
         \     users             integer, \
         \     since             timestamp with time zone not null, \
         \     until             timestamp with time zone not null, \
         \     test              bool not null, \
         \     app_uuid          uuid not null, \
         \     created_at        timestamp with time zone not null, \
         \     updated_at        timestamp with time zone not null \
         \ ); \
         \  \
         \ create unique index app_plan_uuid_uindex \
         \     on app_plan (uuid);"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing
