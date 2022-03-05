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

meta = MigrationMeta {mmNumber = 15, mmName = "Prefab", mmDescription = "Add prefabs"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
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
