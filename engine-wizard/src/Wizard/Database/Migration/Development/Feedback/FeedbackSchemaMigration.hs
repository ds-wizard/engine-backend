module Wizard.Database.Migration.Development.Feedback.FeedbackSchemaMigration where

import Database.PostgreSQL.Simple

import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Util.Logger

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Table/Feedback) started"
  dropTables
  createTables
  logInfo _CMP_MIGRATION "(Table/Feedback) ended"

dropTables = do
  logInfo _CMP_MIGRATION "(Table/Feedback) drop tables"
  let sql = "drop table if exists feedback cascade;"
  let action conn = execute_ conn sql
  runDB action

createTables = do
  logInfo _CMP_MIGRATION "(Table/Feedback) create table"
  let sql =
        " create table feedback \
       \ ( \
       \     uuid uuid not null, \
       \     issue_id int not null, \
       \     question_uuid uuid not null, \
       \     package_id varchar not null, \
       \     title varchar not null, \
       \     content varchar not null, \
       \     created_at timestamptz not null, \
       \     updated_at timestamptz not null \
       \ ); \
       \  \
       \ create unique index feedback_uuid_uindex \
       \     on feedback (uuid); \
       \  \
       \ alter table feedback \
       \     add constraint feedback_pk \
       \         primary key (uuid); \
       \ alter table feedback \
       \    add constraint feedback_package_id_fk \
       \       foreign key (package_id) references package (id); \
       \ create index feedback_package_id_index \
       \    on feedback (package_id); \
       \  \
       \ create index feedback_question_uuid_index \
       \   on feedback (question_uuid); "
  let action conn = execute_ conn sql
  runDB action
