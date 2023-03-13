module Wizard.Database.Migration.Production.Migration_0031_component.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 31, mmName = "Component", mmDescription = "Add component table"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  createComponentTable dbPool
  addPhaseToPackage dbPool
  extendBranch dbPool

createComponentTable dbPool = do
  let sql =
        "CREATE TABLE component \
        \ ( \
        \     name                      varchar not null \
        \         constraint component_pk \
        \             primary key, \
        \     version                   varchar not null,\
        \     built_at timestamp with time zone not null,\
        \     created_at timestamp with time zone not null,\
        \     updated_at timestamp with time zone not null\
        \ ); \
        \  \
        \ CREATE UNIQUE INDEX component_name_uindex \
        \     ON component (name);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

addPhaseToPackage dbPool = do
  let sql =
        "ALTER TABLE package \
        \   ADD phase varchar NOT NULL DEFAULT 'ReleasedPackagePhase';"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

extendBranch dbPool = do
  let sql =
        "ALTER TABLE branch \
        \   ADD version varchar NOT NULL DEFAULT '1.0.0',\
        \   ADD description varchar NOT NULL DEFAULT 'Fill description here',\
        \   ADD readme varchar NOT NULL DEFAULT 'Fill readme here',\
        \   ADD license varchar NOT NULL DEFAULT 'Fill license here';"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
