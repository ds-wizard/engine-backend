module Wizard.Database.Migration.Production.Migration_0025_locale_2.Migration
  ( definition
  ) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 25, mmName = "Locale 2", mmDescription = "Add locale switching"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  changeLocaleShortcutToCode dbPool
  insertDefaultLocale dbPool

changeLocaleShortcutToCode dbPool = do
  let sql =
        "ALTER TABLE locale \
        \     RENAME COLUMN shortcut TO code;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

insertDefaultLocale dbPool = do
  let sql =
        "INSERT INTO locale \
         \VALUES ('b5f6ea5e-89c2-4419-930a-69980bbc36e8', 'English', 'en', true, true, '00000000-0000-0000-0000-000000000000', \
         \        '2022-01-21 00:00:00.000000 +00:00', '2022-01-21 00:00:00.000000 +00:00');"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
