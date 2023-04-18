module Wizard.Database.Migration.Production.Migration_0030_twoFactorAuth.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 30, mmName = "Add 2FA", mmDescription = "Add 2 factor authentication"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  add2fa dbPool
  createTemporaryFileTable dbPool
  renameSupportRepositoryName dbPool
  renameSupportRepositoryUrl dbPool
  addSupportSiteIcon dbPool

add2fa dbPool = do
  let sql =
        "UPDATE app_config \
        \SET authentication = \
        \        jsonb_set( \
        \                to_jsonb(authentication), \
        \                '{internal}', \
        \                concat('{\"registration\":{\"enabled\":', authentication -> 'internal' -> 'registration' -> 'enabled', '}, \"twoFactorAuth\": { \"enabled\": false, \"codeLength\": 6, \"expiration\": 600 }}')::jsonb, \
        \                false \
        \            )"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createTemporaryFileTable dbPool = do
  let sql =
        "CREATE TABLE temporary_file \
        \     ( \
        \         uuid             uuid not null \
        \             CONSTRAINT temporary_file_pk \
        \                 PRIMARY KEY, \
        \         file_name        varchar not null, \
        \         content_type     varchar not null, \
        \         expires_at       timestamp with time zone not null, \
        \         app_uuid uuid default '00000000-0000-0000-0000-000000000000' not null \
        \           CONSTRAINT temporary_file_app_uuid_fk \
        \             REFERENCES app, \
        \         created_by       uuid not null \
        \           CONSTRAINT temporary_file_user_entity_uuid_fk \
        \             REFERENCES user_entity on delete cascade, \
        \         created_at      timestamp with time zone not null \
        \     ); \
        \ CREATE UNIQUE INDEX temporary_file_uuid_uindex \
        \     ON temporary_file (uuid);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

renameSupportRepositoryName dbPool = do
  let sql =
        "UPDATE app_config \
        \SET privacy_and_support = privacy_and_support::jsonb - 'supportRepositoryName' || jsonb_build_object('supportSiteName', privacy_and_support -> 'supportRepositoryName');"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

renameSupportRepositoryUrl dbPool = do
  let sql =
        "UPDATE app_config \
        \SET privacy_and_support = privacy_and_support::jsonb - 'supportRepositoryUrl' || jsonb_build_object('supportSiteUrl', privacy_and_support -> 'supportRepositoryUrl');"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

addSupportSiteIcon dbPool = do
  let sql =
        "UPDATE app_config \
        \SET privacy_and_support = privacy_and_support::jsonb || jsonb_build_object('supportSiteIcon', null);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
