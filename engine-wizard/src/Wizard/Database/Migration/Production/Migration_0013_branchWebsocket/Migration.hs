module Wizard.Database.Migration.Production.Migration_0013_branchWebsocket.Migration
  ( definition
  ) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Data.String (fromString)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 13, mmName = "Branch Websocket", mmDescription = "Add branch websockets"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  createBranchDataTable dbPool
  moveRowsFromBranchToBranchData dbPool
  removeEventsFromBranch dbPool
  addCreatedAtToKmMigration dbPool
  createPersistentCommand dbPool

createBranchDataTable dbPool = do
  let sql =
        "CREATE TABLE branch_data \
        \ ( \
        \     branch_uuid uuid not null \
        \         constraint branch_data_pk \
        \             primary key \
        \         constraint branch_data_branch_uuid_fk \
        \             references branch, \
        \     metamodel_version int not null, \
        \     events json, \
        \     app_uuid uuid default '00000000-0000-0000-0000-000000000000' not null \
        \       constraint branch_app_uuid_fk \
        \         references app, \
        \     created_at timestamp with time zone not null, \
        \     updated_at timestamp with time zone not null \
        \);"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing

moveRowsFromBranchToBranchData dbPool = do
  let sql =
        "INSERT INTO branch_data \
        \SELECT uuid, metamodel_version, events, app_uuid, created_at, updated_at FROM branch;"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing

removeEventsFromBranch dbPool = do
  let sql = "ALTER TABLE branch DROP events, DROP metamodel_version;"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing

addCreatedAtToKmMigration dbPool = do
  let sql = "ALTER TABLE knowledge_model_migration ADD created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now()"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing

createPersistentCommand dbPool = do
  let sql =
        "CREATE TABLE persistent_command \
          \ ( \
          \     uuid uuid not null, \
          \     state varchar not null, \
          \     component varchar not null, \
          \     function varchar not null, \
          \     body varchar not null, \
          \     last_error_message varchar, \
          \     attempts int not null, \
          \     max_attempts int not null, \
          \     app_uuid uuid default '00000000-0000-0000-0000-000000000000' not null \
          \       constraint persistent_command_app_uuid_fk \
          \         references app, \
          \     created_by uuid not null \
          \       constraint persistent_command_created_by_fk \
          \         references user_entity, \
          \     created_at timestamptz not null, \
          \     updated_at timestamptz not null \
          \ ); \
          \  \
          \ CREATE unique index persistent_command_uuid_uindex \
          \     on persistent_command (uuid); \
          \ ALTER TABLE persistent_command \
          \     ADD constraint persistent_command_pk \
          \         primary key (uuid); "
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing
