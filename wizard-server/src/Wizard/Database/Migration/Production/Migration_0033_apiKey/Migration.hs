module Wizard.Database.Migration.Production.Migration_0033_apiKey.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 33, mmName = "API Key", mmDescription = "Add API Key"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  clearUserTokenTable dbPool
  extendUserTokenTable dbPool
  changeDashboardContent dbPool
  renameDashboardColumn dbPool
  addLoginInfoSidebar dbPool

clearUserTokenTable dbPool = do
  let sql = "DELETE FROM user_token WHERE uuid is not null;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

extendUserTokenTable dbPool = do
  let sql =
        "ALTER TABLE user_token \
        \   ADD name varchar NOT NULL,\
        \   ADD type varchar NOT NULL,\
        \   ADD user_agent varchar NOT NULL,\
        \   ADD expires_at timestamp with time zone NOT NULL;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

changeDashboardContent dbPool = do
  let sql =
        "UPDATE app_config \
        \SET dashboard = (jsonb_set( \
        \                        to_jsonb(dashboard), \
        \                        '{announcements}', \
        \                        concat( \
        \                           '[ ', \
        \                           CASE \
        \                               WHEN dashboard ->> 'welcomeInfo' IS NOT NULL THEN concat( \
        \                                       '{ \"content\": ', dashboard -> 'welcomeInfo', \
        \                                       ', \"level\": \"InfoAnnouncementLevelType\", \"dashboard\": true, \"loginScreen\": false } ') \
        \                               ELSE '' \
        \                               END, \
        \                           CASE \
        \                               WHEN dashboard ->> 'welcomeInfo' IS NOT NULL AND dashboard ->> 'welcomeWarning' IS NOT NULL THEN ',' \
        \                               ELSE '' \
        \                               END, \
        \                           CASE \
        \                               WHEN dashboard ->> 'welcomeWarning' IS NOT NULL THEN concat( \
        \                                       '{ \"content\": ', dashboard -> 'welcomeWarning', \
        \                                       ', \"level\": \"WarningAnnouncementLevelType\", \"dashboard\": true, \"loginScreen\": false } ') \
        \                               ELSE '' \
        \                               END, \
        \                           ' ]' \
        \                        )::jsonb \
        \                    )::jsonb \
        \    || jsonb_build_object('loginInfo', look_and_feel -> 'loginInfo')) \
        \    - 'welcomeInfo' \
        \    - 'welcomeWarning'"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

renameDashboardColumn dbPool = do
  let sql =
        "ALTER TABLE app_config \
        \RENAME COLUMN dashboard TO dashboard_and_login_screen;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

addLoginInfoSidebar dbPool = do
  let sql =
        "UPDATE app_config \
        \SET dashboard_and_login_screen = (jsonb_set( \
        \        to_jsonb(dashboard_and_login_screen), \
        \        '{loginInfoSidebar}', \
        \        'null' \
        \    ))::jsonb"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
