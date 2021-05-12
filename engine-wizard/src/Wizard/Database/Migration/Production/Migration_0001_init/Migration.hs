module Wizard.Database.Migration.Production.Migration_0001_init.Migration
  ( definition
  ) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 1, mmName = "Init database", mmDescription = "Create tables and load basic data"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  createUserTable dbPool
  createAclTable dbPool
  createPackageTable dbPool
  createTemplateTable dbPool
  createTemplateFileTable dbPool
  createTemplateAssetTable dbPool
  createQuestionnaireTable dbPool
  createQuestionnaireAclTable dbPool
  createQtnMigrationTable dbPool
  createActionKeyTable dbPool
  createBookReferenceTable dbPool
  createBranchTable dbPool
  createConfigTable dbPool
  createDocumentTable dbPool
  createdDocumentQueueTable dbPool
  createFeedbackTable dbPool
  createLevelTable dbPool
  createMetricTable dbPool
  createKmMigrationTable dbPool
  insertUsers dbPool
  insertAppConfig dbPool
  insertMetrics dbPool
  insertLevels dbPool

createAclTable dbPool =
  createTable
    dbPool
    " create table acl_group \
    \ ( \
    \   id varchar not null, \
    \   name varchar not null, \
    \   description varchar not null \
    \ ); \
    \  \
    \ create unique index acl_group_id_uindex \
    \    on acl_group (id); \
    \  \
    \ alter table acl_group \
    \   add constraint acl_group_pk \
    \      primary key (id); "

createActionKeyTable dbPool =
  createTable
    dbPool
    "create table action_key \
    \     ( \
    \         uuid            uuid not null \
    \             constraint action_key_pk \
    \                 primary key, \
    \         user_id         uuid not null, \
    \         type            varchar not null, \
    \         hash            varchar not null, \
    \         created_at      timestamp with time zone not null \
    \     ); \
    \ create unique index action_key_uuid_uindex \
    \     on action_key (uuid); \
    \ create unique index action_key_hash_uindex \
    \     on action_key (hash);  \
    \ \
    \ alter table action_key \
    \    add constraint action_key_user_entity_uuid_fk \
    \       foreign key (user_id) references user_entity (uuid) on delete cascade;"

createBookReferenceTable dbPool =
  createTable
    dbPool
    " create table book_reference \
     \ ( \
     \   short_uuid varchar not null, \
     \   book_chapter varchar not null, \
     \   content varchar not null, \
     \   created_at timestamptz not null, \
     \   updated_at timestamptz not null \
     \ ); \
     \  \
     \ create unique index book_reference_short_uuid_uindex \
     \    on book_reference (short_uuid); \
     \  \
     \ alter table book_reference \
     \   add constraint book_reference_pk \
     \      primary key (short_uuid); "

createBranchTable dbPool =
  createTable
    dbPool
    " create table branch \
    \ ( \
    \     uuid uuid not null \
    \         constraint branch_pk \
    \             primary key, \
    \     name varchar not null, \
    \     km_id varchar not null, \
    \     metamodel_version int not null, \
    \     previous_package_id varchar \
    \         constraint branch_package_id_fk \
    \             references package, \
    \     events json not null, \
    \     owner_uuid uuid not null \
    \         constraint branch_user_entity_uuid_fk \
    \             references user_entity, \
    \     created_at timestamptz not null, \
    \     updated_at timestamptz not null \
    \ ); \
    \  \
    \ create unique index branch_uuid_uindex \
    \     on branch (uuid); "

createConfigTable dbPool =
  createTable
    dbPool
    " create table app_config \
    \ ( \
    \     id          bigserial        not null, \
    \     organization        json             not null, \
    \     authentication      json             not null, \
    \     privacy_and_support json             not null, \
    \     dashboard           json             not null, \
    \     look_and_feel       json             not null, \
    \     registry            json             not null, \
    \     questionnaire       json             not null, \
    \     template            json             not null, \
    \     submission          json             not null, \
    \     created_at          timestamp with time zone not null, \
    \     updated_at          timestamp with time zone not null \
    \ ); \
    \ create unique index app_config_id_uindex \
    \   on app_config (id); \
    \ alter table app_config \
    \   add constraint app_config_pk \
    \      primary key (id);"

createDocumentTable dbPool =
  createTable
    dbPool
    " create table document \
    \ ( \
    \     uuid uuid not null, \
    \     name varchar not null, \
    \     state varchar not null, \
    \     durability varchar not null, \
    \     questionnaire_uuid uuid not null, \
    \     questionnaire_event_uuid uuid, \
    \     questionnaire_replies_hash bigint not null, \
    \     template_id varchar not null, \
    \     format_uuid uuid not null, \
    \     metadata json not null, \
    \     creator_uuid uuid, \
    \     retrieved_at timestamptz, \
    \     finished_at timestamptz, \
    \     created_at timestamptz not null \
    \ ); \
    \  \
    \ create unique index document_uuid_uindex \
    \     on document (uuid); \
    \  \
    \ alter table document \
    \     add constraint document_pk \
    \         primary key (uuid); \
    \ alter table document \
    \   add constraint document_questionnaire_uuid_fk \
    \      foreign key (questionnaire_uuid) references questionnaire; \
    \  \
    \ alter table document \
    \   add constraint document_template_id_fk \
    \      foreign key (template_id) references template (id); \
    \  \
    \ alter table document \
    \   add constraint document_user_entity_uuid_fk \
    \      foreign key (creator_uuid) references user_entity; \
    \  \
    \ create index document_questionnaire_uuid_index \
    \   on document (questionnaire_uuid);"

createdDocumentQueueTable dbPool =
  createTable
    dbPool
    " create table document_queue \
    \ ( \
    \   id serial not null, \
    \   document_uuid uuid not null \
    \      constraint document_queue_document_uuid_fk \
    \         references document, \
    \   document_context json not null, \
    \   created_by uuid, \
    \   created_at timestamptz not null \
    \ ); \
    \  \
    \ create unique index document_queue_id_uindex \
    \   on document_queue (id); \
    \  \
    \ alter table document_queue \
    \   add constraint document_queue_pk \
    \      primary key (id); "

createFeedbackTable dbPool =
  createTable
    dbPool
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

createLevelTable dbPool =
  createTable
    dbPool
    "create table level \
    \ ( \
    \   level int not null \
    \      constraint level_pk \
    \           primary key, \
    \   title varchar not null, \
    \   description varchar, \
    \   created_at timestamptz not null, \
    \   updated_at timestamptz not null \
    \ ); "

createMetricTable dbPool =
  createTable
    dbPool
    " create table metric \
    \ ( \
    \     uuid uuid not null, \
    \     title varchar not null, \
    \     abbreviation varchar, \
    \     description varchar, \
    \     reference_json json not null, \
    \     created_at timestamptz not null, \
    \     updated_at timestamptz not null \
    \ ); \
    \  \
    \ create unique index metric_uuid_uindex \
    \     on metric (uuid); \
    \  \
    \ alter table metric \
    \     add constraint metric_pk \
    \         primary key (uuid); "

createKmMigrationTable dbPool =
  createTable
    dbPool
    " create table km_migration \
     \ ( \
     \     branch_uuid uuid not null, \
     \     metamodel_version int not null, \
     \     migration_state json not null, \
     \     branch_previous_package_id varchar not null, \
     \     target_package_id varchar not null, \
     \     branch_events json not null, \
     \     target_package_events json not null, \
     \     result_events json not null, \
     \     current_knowledge_model json \
     \ ); \
     \  \
     \ create unique index km_migration_branch_uuid_uindex \
     \     on km_migration (branch_uuid); \
     \  \
     \ alter table km_migration \
     \     add constraint km_migration_pk \
     \         primary key (branch_uuid); \
     \  \
     \ alter table km_migration \
     \   add constraint km_migration_branch_uuid_fk \
     \      foreign key (branch_uuid) references branch; \
     \  \
     \ alter table km_migration \
     \   add constraint km_migration_branch_previous_package_id_fk \
     \      foreign key (branch_previous_package_id) references package (id); \
     \  \
     \ alter table km_migration \
     \   add constraint km_migration_target_package_id_fk \
     \      foreign key (target_package_id) references package (id); "

createQtnMigrationTable dbPool =
  createTable
    dbPool
    " create table qtn_migration \
    \ ( \
    \   old_questionnaire_uuid uuid not null, \
    \   new_questionnaire_uuid uuid not null, \
    \   resolved_question_uuids json not null, \
    \   constraint qtn_migration_pk \
    \      primary key (old_questionnaire_uuid, new_questionnaire_uuid) \
    \ ); \
    \  \
    \ alter table qtn_migration \
    \   add constraint qtn_migration_old_questionnaire_uuid_fk \
    \      foreign key (old_questionnaire_uuid) references questionnaire; \
    \  \
    \ alter table qtn_migration \
    \   add constraint qtn_migration_new_questionnaire_uuid_fk \
    \      foreign key (new_questionnaire_uuid) references questionnaire; "

createPackageTable dbPool =
  createTable
    dbPool
    "create table package \
    \ ( \
    \     id                  varchar          not null \
    \         constraint package_pk \
    \             primary key, \
    \     name                varchar          not null, \
    \     organization_id             varchar          not null, \
    \     km_id               varchar          not null, \
    \     version             varchar          not null, \
    \     metamodel_version           integer          not null, \
    \     description         varchar          not null, \
    \     readme              varchar          not null, \
    \     license             varchar          not null, \
    \     previous_package_id         varchar, \
    \     fork_of_package_id          varchar, \
    \     merge_checkpoint_package_id varchar, \
    \     events              json             not null, \
    \     created_at          timestamp with time zone not null \
    \ ); \
    \create unique index package_id_uindex \
    \     on package (id); \
    \create index package_organization_id_km_id_index \
    \     on package (organization_id, km_id); \
    \create index package_previous_package_id_index \
    \     on package (previous_package_id); \
    \  \
    \alter table package \
    \   add constraint package_previous_package_id_fk \
    \      foreign key (previous_package_id) references package; \
    \  \
    \alter table package \
    \   add constraint package_fork_of_package_id_fk \
    \      foreign key (fork_of_package_id) references package; \
    \  \
    \alter table package \
    \   add constraint package_merge_checkpoint_package_id_fk \
    \      foreign key (merge_checkpoint_package_id) references package; "

createQuestionnaireTable dbPool =
  createTable
    dbPool
    "create table questionnaire \
    \ ( \
    \     uuid uuid not null, \
    \     name varchar not null, \
    \     visibility varchar not null, \
    \     sharing varchar not null, \
    \     package_id varchar not null, \
    \     selected_tag_uuids json not null, \
    \     template_id varchar, \
    \     format_uuid uuid, \
    \     creator_uuid uuid, \
    \     events json not null, \
    \     versions json not null, \
    \     created_at timestamptz not null, \
    \     updated_at timestamptz not null \
    \ ); \
    \  \
    \ create unique index questionnaire_uuid_uindex \
    \     on questionnaire (uuid); \
    \  \
    \ alter table questionnaire \
    \     add constraint questionnaire_pk \
    \         primary key (uuid); \
    \ alter table questionnaire \
    \   add constraint questionnaire_package_id_fk \
    \      foreign key (package_id) references package (id); \
    \  \
    \ alter table questionnaire \
    \   add constraint questionnaire_template_id_fk \
    \      foreign key (template_id) references template (id); \
    \  \
    \ alter table questionnaire \
    \   add constraint questionnaire_user_entity_uuid_fk \
    \      foreign key (creator_uuid) references user_entity; "

createQuestionnaireAclTable dbPool =
  createTable
    dbPool
    "create table questionnaire_acl_user \
    \ ( \
    \     uuid               uuid   not null \
    \         constraint questionnaire_user_acl_pk \
    \             primary key, \
    \     user_uuid          uuid   not null \
    \         constraint questionnaire_acl_user_user_uuid_fk \
    \             references user_entity on delete cascade, \
    \     perms              text[] not null, \
    \     questionnaire_uuid uuid   not null \
    \         constraint questionnaire_acl_user_questionnaire_uuid_fk \
    \             references questionnaire on delete cascade \
    \ ); \
    \  \
    \ create unique index questionnaire_acl_user_uuid_uindex \
    \     on questionnaire_acl_user (uuid); \
    \  \
    \ create table questionnaire_acl_group \
    \ ( \
    \     uuid               uuid    not null \
    \         constraint questionnaire_acl_group_pk \
    \             primary key, \
    \     group_id           varchar not null \
    \         constraint questionnaire_acl_group_group_id_fk \
    \             references acl_group on delete cascade, \
    \     perms              text[]  not null, \
    \     questionnaire_uuid uuid    not null \
    \         constraint questionnaire_acl_group_questionnaire_uuid_fk \
    \             references questionnaire on delete cascade \
    \ ); \
    \  \
    \ create unique index questionnaire_acl_group_uuid_uindex \
    \     on questionnaire_acl_group (uuid); "

createTemplateTable dbPool =
  createTable
    dbPool
    "create table template \
          \ ( \
          \     id                     varchar                  not null \
          \         constraint template_pk \
          \             primary key, \
          \     name                   varchar                  not null, \
          \     organization_id        varchar                  not null, \
          \     template_id            varchar                  not null, \
          \     version                varchar                  not null, \
          \     metamodel_version      integer                  not null, \
          \     description            varchar                  not null, \
          \     readme                 varchar                  not null, \
          \     license                varchar                  not null, \
          \     allowed_packages       json                     not null, \
          \     recommended_package_id varchar, \
          \     formats                json                     not null, \
          \     created_at             timestamp with time zone not null \
          \ ); \
          \create unique index template_id_uindex \
          \     on template (id); \
          \create index template_organization_id_template_id_index \
          \     on template (organization_id, template_id); "

createTemplateFileTable dbPool =
  createTable
    dbPool
    " create table template_file \
        \ ( \
        \   template_id varchar not null, \
        \   uuid uuid not null, \
        \   file_name varchar not null, \
        \   content varchar not null \
        \ ); \
        \  \
        \ alter table template_file \
        \   add constraint template_file_template_id_fk \
        \      foreign key (template_id) references template (id); \
        \  \
        \ create unique index template_file_uuid_uindex \
        \   on template_file (uuid); \
        \  \
        \ alter table template_file \
        \   add constraint template_file_pk \
        \      primary key (uuid); "

createTemplateAssetTable dbPool =
  createTable
    dbPool
    " create table template_asset \
        \ ( \
        \   template_id varchar not null, \
        \   uuid uuid not null, \
        \   file_name varchar not null, \
        \   content_type varchar not null \
        \ ); \
        \  \
        \ alter table template_asset \
        \   add constraint template_asset_template_id_fk \
        \      foreign key (template_id) references template (id); \
        \  \
        \ create unique index template_asset_uuid_uindex \
        \   on template_asset (uuid); \
        \  \
        \ alter table template_asset \
        \   add constraint template_asset_pk \
        \      primary key (uuid); "

createUserTable dbPool =
  createTable
    dbPool
    " create table user_entity \
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
    \     updated_at        timestamp with time zone not null \
    \ ); \
    \  \
    \ create unique index user_email_uindex \
    \     on user_entity (email); \
    \  \
    \ create unique index user_uuid_uindex \
    \     on user_entity (uuid); "

insertUsers dbPool = do
  let sql =
        "INSERT INTO user_entity (uuid, \
        \                         first_name, \
        \                         last_name, \
        \                         email, \
        \                         password_hash, \
        \                         affiliation, \
        \                         sources, \
        \                         role, \
        \                         permissions, \
        \                         active, \
        \                         submissions_props, \
        \                         image_url, \
        \                         groups, \
        \                         last_visited_at, \
        \                         created_at, \
        \                         updated_at) \
        \VALUES ('ec6f8e90-2a91-49ec-aa3f-9eab2267fc66', \
        \        'Albert', \
        \        'Einstein', \
        \        'albert.einstein@example.com', \
        \        'sha256|17|awVwfF3h27PrxINtavVgFQ==|iUFbQnZFv+rBXBu1R2OkX+vEjPtohYk5lsyIeOBdEy4=', \
        \        'My University', \
        \        '[\"internal\"]', \
        \        'admin', \
        \        '{\"UM_PERM\",\"KM_PERM\",\"KM_UPGRADE_PERM\",\"KM_PUBLISH_PERM\",\"PM_READ_PERM\",\"PM_WRITE_PERM\",\"QTN_PERM\",\"DMP_PERM\",\"CFG_PERM\",\"SUBM_PERM\",\"TML_PERM\",\"DOC_PERM\"}', \
        \        true, \
        \        '[]', \
        \        null, \
        \        '[]', \
        \        '2018-01-20 00:00:00.000000', \
        \        '2018-01-20 00:00:00.000000', \
        \        '2018-01-25 00:00:00.000000'); \
        \INSERT INTO user_entity (uuid, \
        \                         first_name, \
        \                         last_name, \
        \                         email, \
        \                         password_hash, \
        \                         affiliation, \
        \                         sources, \
        \                         role, \
        \                         permissions, \
        \                         active, \
        \                         submissions_props, \
        \                         image_url, \
        \                         groups, \
        \                         last_visited_at, \
        \                         created_at, \
        \                         updated_at) \
        \VALUES ('30d48cf4-8c8a-496f-bafe-585bd238f798', \
        \        'Nikola', \
        \        'Tesla', \
        \        'nikola.tesla@example.com', \
        \        'sha256|17|awVwfF3h27PrxINtavVgFQ==|iUFbQnZFv+rBXBu1R2OkX+vEjPtohYk5lsyIeOBdEy4=', \
        \        null, \
        \        '[\"internal\"]', \
        \        'dataSteward', \
        \        '{\"KM_PERM\",\"KM_UPGRADE_PERM\",\"KM_PUBLISH_PERM\",\"PM_READ_PERM\",\"PM_WRITE_PERM\",\"QTN_PERM\",\"DMP_PERM\",\"SUBM_PERM\",\"TML_PERM\"}', \
        \        true, \
        \        '[]', \
        \        null, \
        \        '[]', \
        \        '2018-01-20 00:00:00.000000', \
        \        '2018-01-20 00:00:00.000000', \
        \        '2018-01-25 00:00:00.000000'); \
        \INSERT INTO user_entity (uuid, \
        \                         first_name, \
        \                         last_name, \
        \                         email, \
        \                         password_hash, \
        \                         affiliation, \
        \                         sources, \
        \                         role, \
        \                         permissions, \
        \                         active, \
        \                         submissions_props, \
        \                         image_url, \
        \                         groups, \
        \                         last_visited_at, \
        \                         created_at, \
        \                         updated_at) \
        \VALUES ('e1c58e52-0824-4526-8ebe-ec38eec67030', \
        \        'Isaac', \
        \        'Newton', \
        \        'isaac.newton@example.com', \
        \        'sha256|17|awVwfF3h27PrxINtavVgFQ==|iUFbQnZFv+rBXBu1R2OkX+vEjPtohYk5lsyIeOBdEy4=', \
        \        null, \
        \        '[\"internal\"]', \
        \        'researcher', \
        \        '{\"PM_READ_PERM\",\"QTN_PERM\",\"DMP_PERM\",\"SUBM_PERM\"}', \
        \        true, \
        \        '[]', \
        \        null, \
        \        '[]', \
        \        '2018-01-20 00:00:00.000000', \
        \        '2018-01-20 00:00:00.000000', \
        \        '2018-01-25 00:00:00.000000');"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

insertAppConfig dbPool = do
  let sql =
        "INSERT INTO app_config (id, organization, authentication, privacy_and_support, dashboard, look_and_feel, \
        \                        registry, questionnaire, template, submission, created_at, updated_at) \
        \VALUES (1, \
        \        '{ \
        \          \"affiliations\": [], \
        \          \"name\": \"My Organization\", \
        \          \"organizationId\": \"myorg\", \
        \          \"description\": \"My Organization Description\" \
        \        }', \
        \        '{ \
        \          \"internal\": { \
        \            \"registration\": { \
        \              \"enabled\": true \
        \            } \
        \          }, \
        \          \"external\": { \
        \            \"services\": [] \
        \          }, \
        \          \"defaultRole\": \"dataSteward\" \
        \        }', \
        \        '{ \
        \          \"supportRepositoryUrl\": null, \
        \          \"supportRepositoryName\": null, \
        \          \"termsOfServiceUrl\": null, \
        \          \"privacyUrl\": null, \
        \          \"supportEmail\": null \
        \        }', \
        \        '{ \
        \          \"welcomeWarning\": null, \
        \          \"welcomeInfo\": null, \
        \          \"widgets\": { \
        \            \"admin\": [ \
        \              \"DMPWorkflow\", \
        \              \"LevelsQuestionnaire\" \
        \            ], \
        \            \"dataSteward\": [ \
        \              \"DMPWorkflow\", \
        \              \"LevelsQuestionnaire\" \
        \            ], \
        \            \"researcher\": [ \
        \              \"DMPWorkflow\", \
        \              \"LevelsQuestionnaire\" \
        \            ] \
        \          } \
        \        }', \
        \        '{ \
        \          \"appTitleShort\": null, \
        \          \"appTitle\": null, \
        \          \"loginInfo\": null, \
        \          \"customMenuLinks\": [] \
        \        }', \
        \        '{ \
        \          \"enabled\": false, \
        \          \"token\": \"\" \
        \        }', \
        \        '{ \
        \          \"questionnaireVisibility\": { \
        \            \"enabled\": true, \
        \            \"defaultValue\": \"PrivateQuestionnaire\" \
        \          }, \
        \          \"questionnaireSharing\": { \
        \            \"enabled\": true, \
        \            \"defaultValue\": \"RestrictedQuestionnaire\" \
        \          }, \
        \          \"summaryReport\": { \
        \            \"enabled\": true \
        \          }, \
        \          \"levels\": { \
        \            \"enabled\": true \
        \          }, \
        \          \"feedback\": { \
        \            \"enabled\": false, \
        \            \"token\": \"\", \
        \            \"owner\": \"\", \
        \            \"repo\": \"\" \
        \          } \
        \        }', \
        \        '{ \
        \          \"recommendedTemplateId\": null \
        \        }', \
        \        '{ \
        \          \"enabled\": true, \
        \          \"services\": [] \
        \        }', \
        \        '2018-01-20 00:00:00.000000', \
        \        '2018-01-20 00:00:00.000000');"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

insertMetrics dbPool = do
  let sql =
        "INSERT INTO metric (uuid, title, abbreviation, description, reference_json, created_at, updated_at) VALUES ('8db30660-d4e5-4c0a-bf3e-553f3f0f997a', 'Findability', 'F', 'The Findability metric describes how easily data can be located. The score associated with an answer will be higher if it makes it easier for humans or for computers to locate your data set, e.g. if it ends up in an index or has a unique resolvable identifier.', '[]', '2018-01-20 00:00:00.000000', '2018-01-21 00:00:00.000000'); \
        \INSERT INTO metric (uuid, title, abbreviation, description, reference_json, created_at, updated_at) VALUES ('0feac7e6-add4-4723-abae-be5ce7864c63', 'Accessibility', 'A', 'The Accessibility metric describes how well the access to the database is described and how easy it is to implement. The score associated with an answer will be higher if it makes it easier for humans and computers to get to the data. This is determined by e.g. the protocol for accessing the data or for authenticating users, and also by the guaranteed longevity of the repository. Note that this is different from the Openness metric!', '[]', '2018-01-20 00:00:00.000000', '2018-01-21 00:00:00.000000'); \
        \INSERT INTO metric (uuid, title, abbreviation, description, reference_json, created_at, updated_at) VALUES ('a42bded3-a085-45f8-b384-32b4a77c8385', 'Interoperability', 'I', 'The Interoperability metric describes how well the data interoperates with other data. The score associated with an answer will be higher if it makes it easier for humans and computers to couple the data with other data and ''understand'' relationships. This is influenced by the use of standard ontologies for different fields and proper descriptions of the relations. It is also influenced by proper standard metadata that is agreed by the community.', '[]', '2018-01-20 00:00:00.000000', '2018-01-21 00:00:00.000000'); \
        \INSERT INTO metric (uuid, title, abbreviation, description, reference_json, created_at, updated_at) VALUES ('0bafe0c2-a8f2-4c74-80c8-dbf3a5b8e9b7', 'Reusability', 'R', 'The Reusability metric describes how well the data is suitable for reuse in other context. The score associated with an answer will be higher if it makes it easier for humans and computers to reuse the data. This is influenced largely by proper description of how the data was obtained, and also by the conditions that are put on the reuse (license and, for personally identifying information, consent).', '[]', '2018-01-20 00:00:00.000000', '2018-01-21 00:00:00.000000'); \
        \INSERT INTO metric (uuid, title, abbreviation, description, reference_json, created_at, updated_at) VALUES ('8845fe2b-79df-4138-baea-3a035bf5e249', 'Good DMP Practice', 'G', 'The Good DMP Practice metric describes how appreciated a process is among Data Stewards. A score associated with an answer will be high if a practice would be considered preferable over alternatives, generally a good idea.', '[]', '2018-01-20 00:00:00.000000', '2018-01-21 00:00:00.000000'); \
        \INSERT INTO metric (uuid, title, abbreviation, description, reference_json, created_at, updated_at) VALUES ('cc02c5a0-9754-4432-a7e0-ce0f3cf7a0a0', 'Openness', 'O', 'The Openness metric describes how Open the data are available. Note that this is different from the Accessibility metric. A score associated with an answer will be high if the data will be as open as possible, and low if voluntary restrictions apply to access and re-use.', '[]', '2018-01-20 00:00:00.000000', '2018-01-21 00:00:00.000000'); "
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

insertLevels dbPool = do
  let sql =
        "INSERT INTO level (level, title, description, created_at, updated_at) VALUES (1, 'Before Submitting the Proposal', '', '2018-01-20 00:00:00.000000', '2018-01-21 00:00:00.000000'); \
        \INSERT INTO level (level, title, description, created_at, updated_at) VALUES (2, 'Before Submitting the DMP', '', '2018-01-20 00:00:00.000000', '2018-01-21 00:00:00.000000'); \
        \INSERT INTO level (level, title, description, created_at, updated_at) VALUES (3, 'Before Finishing the Project', '', '2018-01-20 00:00:00.000000', '2018-01-21 00:00:00.000000');"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

createTable dbPool sql = do
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
