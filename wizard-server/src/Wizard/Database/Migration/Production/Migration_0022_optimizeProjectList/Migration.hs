module Wizard.Database.Migration.Production.Migration_0022_optimizeProjectList.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Data.String (fromString)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 22, mmName = "Optimize project list", mmDescription = "Optimize project list endpoint"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  addIndicationsToQuestionnaire dbPool
  addSystemUser dbPool
  createCommentTable dbPool
  createMoveQuestionnaireCommentsToSeparateTableCommand dbPool

addIndicationsToQuestionnaire dbPool = do
  let sql =
        "ALTER TABLE questionnaire \
        \    ADD answered_questions int not null default 0, \
        \    ADD unanswered_questions int not null default 0"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

addSystemUser dbPool = do
  -- cspell:disable
  let sql =
        "INSERT INTO user_entity \
        \VALUES ('00000000-0000-0000-0000-000000000000', \
        \        'System', \
        \        'User', \
        \        'system@example.com', \
        \        'pbkdf1:sha256|17|awVwfF3h27PrxINtavVgFQ==|iUFbQnZFv+rBXBu1R2OkX+vEjPtohYk5lsyIeOBdEy4=', \
        \        null, '[\"internal\"]', \
        \        'admin',  \
        \        '{ADMIN_PERM,UM_PERM,KM_PERM,KM_UPGRADE_PERM,KM_PUBLISH_PERM,PM_READ_PERM,PM_WRITE_PERM,QTN_PERM,QTN_TML_PERM,DMP_PERM,CFG_PERM,SUBM_PERM,TML_PERM,DOC_PERM}', \
        \        true, \
        \        '[]', \
        \        null, \
        \        '[]', \
        \        '2018-01-20 00:00:00.000000 +00:00', \
        \        '2018-01-20 00:00:00.000000 +00:00', \
        \        '2018-01-25 00:00:00.000000 +00:00', \
        \        '00000000-0000-0000-0000-000000000000', \
        \        true);"
  -- cspell:enable
  let action conn = execute_ conn (fromString sql)
  liftIO $ withResource dbPool action
  return Nothing

createCommentTable dbPool = do
  let sql =
        "create table questionnaire_comment_thread \
        \( \
        \    uuid       uuid        not null \
        \        constraint questionnaire_comment_thread_pk \
        \            primary key, \
        \    path       text        not null, \
        \    resolved   bool        not null, \
        \    private    bool        not null, \
        \    questionnaire_uuid uuid   not null \
        \        constraint questionnaire_comment_thread_questionnaire_uuid_fk \
        \            references questionnaire, \
        \    created_by uuid \
        \        constraint questionnaire_comment_thread_user_entity_uuid_fk \
        \            references user_entity, \
        \    created_at timestamptz not null, \
        \    updated_at timestamptz not null \
        \); \
        \ \
        \create unique index questionnaire_comment_thread_uuid_uindex \
        \    on questionnaire_comment_thread (uuid); \
        \ \
        \create table questionnaire_comment \
        \( \
        \    uuid       uuid        not null \
        \        constraint questionnaire_comment_pk \
        \            primary key, \
        \    text   text        not null, \
        \    comment_thread_uuid uuid \
        \        constraint questionnaire_comment_questionnaire_comment_thread_uuid_fk \
        \            references questionnaire_comment_thread, \
        \    created_by uuid \
        \        constraint questionnaire_comment_user_entity_uuid_fk \
        \            references user_entity, \
        \    created_at timestamptz not null, \
        \    updated_at timestamptz not null \
        \); \
        \ \
        \create unique index questionnaire_comment_uuid_uindex \
        \    on questionnaire_comment (uuid); "
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createMoveQuestionnaireCommentsToSeparateTableCommand dbPool = do
  let sql =
        "INSERT INTO persistent_command \
        \VALUES ('8a30e536-c863-42d9-b931-b0e8ada3fc29', \
        \        'NewPersistentCommandState', \
        \        'Questionnaire', \
        \        'moveQuestionnaireCommentsToSeparateTable', \
        \        'empty', \
        \        null, \
        \        0, \
        \        10, \
        \        '00000000-0000-0000-0000-000000000000', \
        \        '00000000-0000-0000-0000-000000000000', \
        \        now(), \
        \        now(), \
        \        true);"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
