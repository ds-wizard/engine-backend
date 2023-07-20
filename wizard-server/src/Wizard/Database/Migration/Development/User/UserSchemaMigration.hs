module Wizard.Database.Migration.Development.User.UserSchemaMigration where

import Database.PostgreSQL.Simple

import Shared.Common.Util.Logger
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Table/User) started"
  dropTables
  createTables
  logInfo _CMP_MIGRATION "(Table/User) ended"

dropTables = do
  logInfo _CMP_MIGRATION "(Table/User) drop tables"
  let sql =
        "drop table if exists user_token cascade;\
        \drop table if exists user_entity cascade;"
  let action conn = execute_ conn sql
  runDB action

createTables = do
  createUserTable
  createUserTokenTable

createUserTable = do
  logInfo _CMP_MIGRATION "(Table/User) create tables"
  let sql =
        "create table user_entity \
        \ ( \
        \     uuid              uuid    not null \
        \         constraint user_pk \
        \             primary key, \
        \     first_name        varchar not null, \
        \     last_name         varchar not null, \
        \     email             varchar not null, \
        \     password_hash     varchar not null, \
        \     affiliation       varchar, \
        \     sources           json    not null, \
        \     role              varchar not null, \
        \     permissions       text[]  not null, \
        \     active            boolean not null, \
        \     submissions_props json    not null, \
        \     image_url         varchar, \
        \     groups            json    not null, \
        \     last_visited_at   timestamp with time zone not null, \
        \     created_at        timestamp with time zone not null, \
        \     updated_at        timestamp with time zone not null, \
        \     app_uuid uuid default '00000000-0000-0000-0000-000000000000' not null \
        \         constraint user_entity_app_uuid_fk \
        \             references app, \
        \     machine           boolean not null default false \
        \ ); \
        \  \
        \ create unique index user_uuid_uindex \
        \     on user_entity (uuid, app_uuid); \
        \  \
        \ create unique index user_email_uindex \
        \     on user_entity (email, app_uuid); "
  let action conn = execute_ conn sql
  runDB action

createUserTokenTable = do
  logInfo _CMP_MIGRATION "(Table/UserToken) create tables"
  let sql =
        "create table user_token \
        \ ( \
        \     uuid              uuid    not null \
        \         constraint user_token_pk \
        \             primary key, \
        \     user_uuid uuid not null \
        \         constraint user_token_user_uuid_fk \
        \             references user_entity, \
        \     value             varchar not null, \
        \     session_state     varchar, \
        \     app_uuid uuid default '00000000-0000-0000-0000-000000000000' not null \
        \         constraint user_entity_app_uuid_fk \
        \             references app, \
        \     created_at        timestamp with time zone not null, \
        \     name              varchar not null, \
        \     type              varchar not null, \
        \     user_agent        varchar not null, \
        \     expires_at        timestamp with time zone not null \
        \ ); \
        \  \
        \ create unique index user_token_uuid_uindex \
        \     on user_token (uuid, app_uuid);"
  let action conn = execute_ conn sql
  runDB action
