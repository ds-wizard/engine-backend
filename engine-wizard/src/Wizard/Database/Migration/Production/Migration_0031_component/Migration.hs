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
  changeReadmeInDefaultLocale dbPool

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

changeReadmeInDefaultLocale dbPool = do
  let sql =
        "UPDATE locale \
        \SET description = 'Default English locale for Wizard UI', \
        \    readme      = concat('# Default English Locale for Wizard Client', \
        \              CHR(13), \
        \              CHR(10), \
        \              CHR(13), \
        \              CHR(10), \
        \              '[![Language](https://img.shields.io/badge/ISO%20639--1-en-blue)](https://en.wikipedia.org/wiki/English_language)', \
        \              CHR(13), \
        \              CHR(10), \
        \              CHR(13), \
        \              CHR(10), \
        \              'This is the default English locale embedded in the Wizard Client. Therefore, it is always complete and compatible with the version that it is shipped with.', \
        \              CHR(13), \
        \              CHR(10), \
        \              CHR(13), \
        \              CHR(10), \
        \              'The locale also cannot be exported or deleted. However, you can *Disable* it anytime as well as mark other locale to be used as *Default* if necessary.', \
        \              CHR(13), \
        \              CHR(10), \
        \              CHR(13), \
        \              CHR(10), \
        \              'In case you encounter any issues with this issue, please contact your service provider.', \
        \              CHR(13), \
        \              CHR(10) \
        \           ) \
        \WHERE id = 'wizard:default:1.0.0';"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
