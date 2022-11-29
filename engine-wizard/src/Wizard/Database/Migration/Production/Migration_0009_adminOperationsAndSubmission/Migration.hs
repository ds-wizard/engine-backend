module Wizard.Database.Migration.Production.Migration_0009_adminOperationsAndSubmission.Migration (
  definition,
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
    { mmNumber = 9
    , mmName = "Admin Operations And Submission"
    , mmDescription = "Add ADMIN_PERM to admin users and enhance submission"
    }

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  addAdminPerm dbPool
  addSubmissionTable dbPool

addAdminPerm dbPool = do
  let sql = "UPDATE user_entity set permissions = permissions || '{ADMIN_PERM}' WHERE role = 'admin'"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing

addSubmissionTable dbPool = do
  let sql =
        "create table submission \
        \ ( \
        \     uuid uuid not null, \
        \     state varchar not null, \
        \     location varchar, \
        \     returned_data varchar, \
        \     service_id varchar not null, \
        \     document_uuid uuid, \
        \     created_by uuid, \
        \     created_at timestamptz, \
        \     updated_at timestamptz not null \
        \ ); \
        \  \
        \ create unique index submission_uuid_uindex \
        \     on submission (uuid); \
        \  \
        \ alter table submission \
        \     add constraint submission_pk \
        \         primary key (uuid); \
        \ alter table submission \
        \   add constraint submission_document_uuid_fk \
        \      foreign key (document_uuid) references document (uuid); \
        \  \
        \ alter table submission \
        \   add constraint submission_created_by_fk \
        \      foreign key (created_by) references user_entity (uuid); \
        \  \
        \ create index submission_document_uuid_index \
        \   on submission (document_uuid);"
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing
