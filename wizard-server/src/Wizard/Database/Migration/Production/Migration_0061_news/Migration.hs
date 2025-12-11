module Wizard.Database.Migration.Production.Migration_0061_news.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 61, mmName = "Add user news", mmDescription = "Add user news feature"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  addLastSeeNewsIdColumnToUser dbPool
  renameQuestionnaireToProject dbPool
  renameAuditLogsAndPersistentCommands dbPool

addLastSeeNewsIdColumnToUser dbPool = do
  let sql = "ALTER TABLE user_entity ADD COLUMN last_seen_news_id varchar;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

renameQuestionnaireToProject dbPool = do
  let sql =
        "ALTER TABLE questionnaire_action RENAME TO project_action; \
        \ALTER TABLE project_action RENAME CONSTRAINT questionnaire_action_pk TO project_action_pk; \
        \ALTER TABLE project_action RENAME CONSTRAINT questionnaire_action_tenant_uuid_fk TO project_action_tenant_uuid_fk; \
        \ \
        \ALTER TABLE questionnaire RENAME TO project; \
        \ALTER TABLE project RENAME CONSTRAINT questionnaire_pk TO project_pk; \
        \ALTER TABLE project RENAME CONSTRAINT questionnaire_created_by_fk TO project_created_by_fk; \
        \ALTER TABLE project RENAME CONSTRAINT questionnaire_document_template_id_fk TO project_document_template_id_fk; \
        \ALTER TABLE project RENAME CONSTRAINT questionnaire_knowledge_model_package_id_fk TO project_knowledge_model_package_id_fk; \
        \ALTER TABLE project RENAME CONSTRAINT questionnaire_tenant_uuid_fk TO project_tenant_uuid_fk; \
        \ \
        \ALTER TABLE questionnaire_importer RENAME TO project_importer; \
        \ALTER TABLE project_importer RENAME CONSTRAINT questionnaire_importer_pk TO project_importer_pk; \
        \ALTER TABLE project_importer RENAME CONSTRAINT questionnaire_importer_tenant_uuid_fk TO project_importer_tenant_uuid_fk; \
        \ \
        \ALTER TABLE questionnaire_event RENAME TO project_event; \
        \ALTER TABLE project_event RENAME COLUMN questionnaire_uuid TO project_uuid; \
        \ALTER TABLE project_event RENAME CONSTRAINT questionnaire_event_pk TO project_event_pk; \
        \ALTER TABLE project_event RENAME CONSTRAINT questionnaire_event_created_by_fk TO project_event_created_by_fk; \
        \ALTER TABLE project_event RENAME CONSTRAINT questionnaire_event_tenant_uuid_fk TO project_event_tenant_uuid_fk; \
        \ALTER TABLE project_event RENAME CONSTRAINT questionnaire_event_questionnaire_uuid_fk TO project_event_project_uuid_fk; \
        \ \
        \ALTER TABLE questionnaire_migration RENAME TO project_migration; \
        \ALTER TABLE project_migration RENAME COLUMN old_questionnaire_uuid TO old_project_uuid; \
        \ALTER TABLE project_migration RENAME COLUMN new_questionnaire_uuid TO new_project_uuid; \
        \ALTER TABLE project_migration RENAME CONSTRAINT questionnaire_migration_pk TO project_migration_pk; \
        \ALTER TABLE project_migration RENAME CONSTRAINT questionnaire_migration_new_questionnaire_uuid_fk TO project_migration_new_project_uuid_fk; \
        \ALTER TABLE project_migration RENAME CONSTRAINT questionnaire_migration_old_questionnaire_uuid_fk TO project_migration_old_project_uuid_fk; \
        \ALTER TABLE project_migration RENAME CONSTRAINT questionnaire_migration_tenant_uuid_fk TO project_migration_tenant_uuid_fk; \
        \ \
        \ALTER TABLE questionnaire_file RENAME TO project_file; \
        \ALTER TABLE project_file RENAME COLUMN questionnaire_uuid TO project_uuid; \
        \ALTER TABLE project_file RENAME CONSTRAINT questionnaire_file_pk TO project_file_pk; \
        \ALTER TABLE project_file RENAME CONSTRAINT questionnaire_file_questionnaire_uuid_fk TO project_file_project_uuid_fk; \
        \ALTER TABLE project_file RENAME CONSTRAINT questionnaire_file_created_by_fk TO project_file_created_by_fk; \
        \ALTER TABLE project_file RENAME CONSTRAINT questionnaire_file_tenant_uuid_fk TO project_file_tenant_uuid_fk; \
        \ALTER TRIGGER trigger_on_after_questionnaire_file_delete ON project_file RENAME TO trigger_on_after_project_file_delete; \
        \ALTER FUNCTION create_persistent_command_from_questionnaire_file_delete() RENAME TO create_persistent_command_from_project_file_delete; \
        \ \
        \ALTER TABLE questionnaire_perm_user RENAME TO project_perm_user; \
        \ALTER TABLE project_perm_user RENAME COLUMN questionnaire_uuid TO project_uuid; \
        \ALTER TABLE project_perm_user RENAME CONSTRAINT questionnaire_perm_user_pk TO project_perm_user_pk; \
        \ALTER TABLE project_perm_user RENAME CONSTRAINT questionnaire_perm_user_questionnaire_uuid_fk TO project_perm_user_project_uuid_fk; \
        \ALTER TABLE project_perm_user RENAME CONSTRAINT questionnaire_perm_user_tenant_uuid_fk TO project_perm_user_tenant_uuid_fk; \
        \ALTER TABLE project_perm_user RENAME CONSTRAINT questionnaire_perm_user_user_uuid_fk TO project_perm_user_user_uuid_fk; \
        \ \
        \ALTER TABLE questionnaire_perm_group RENAME TO project_perm_group; \
        \ALTER TABLE project_perm_group RENAME COLUMN questionnaire_uuid TO project_uuid; \
        \ALTER TABLE project_perm_group RENAME CONSTRAINT questionnaire_perm_group_pk TO project_perm_group_pk; \
        \ALTER TABLE project_perm_group RENAME CONSTRAINT questionnaire_perm_group_questionnaire_uuid_fk TO project_perm_group_project_uuid_fk; \
        \ALTER TABLE project_perm_group RENAME CONSTRAINT questionnaire_perm_group_tenant_uuid_fk TO project_perm_group_tenant_uuid_fk; \
        \ALTER TABLE project_perm_group RENAME CONSTRAINT questionnaire_perm_group_user_group_uuid_fk TO project_perm_group_user_group_uuid_fk; \
        \ \
        \ALTER TABLE questionnaire_version RENAME TO project_version; \
        \ALTER TABLE project_version RENAME COLUMN questionnaire_uuid TO project_uuid; \
        \ALTER TABLE project_version RENAME CONSTRAINT questionnaire_version_pk TO project_version_pk; \
        \ALTER TABLE project_version RENAME CONSTRAINT questionnaire_version_event_uuid_fk TO project_version_event_uuid_fk; \
        \ALTER TABLE project_version RENAME CONSTRAINT questionnaire_version_created_by_fk TO project_version_created_by_fk; \
        \ALTER TABLE project_version RENAME CONSTRAINT questionnaire_version_questionnaire_uuid_fk TO project_version_questionnaire_uuid_fk; \
        \ALTER TABLE project_version RENAME CONSTRAINT questionnaire_version_tenant_uuid_fk TO project_version_tenant_uuid_fk; \
        \ \
        \ALTER TABLE questionnaire_comment_thread RENAME TO project_comment_thread; \
        \ALTER TABLE project_comment_thread RENAME COLUMN questionnaire_uuid TO project_uuid; \
        \ALTER TABLE project_comment_thread RENAME CONSTRAINT questionnaire_comment_thread_pk TO project_comment_thread_pk; \
        \ALTER TABLE project_comment_thread RENAME CONSTRAINT questionnaire_comment_thread_assigned_by TO project_comment_thread_assigned_by; \
        \ALTER TABLE project_comment_thread RENAME CONSTRAINT questionnaire_comment_thread_assigned_to TO project_comment_thread_assigned_to; \
        \ALTER TABLE project_comment_thread RENAME CONSTRAINT questionnaire_comment_thread_questionnaire_uuid TO project_comment_thread_questionnaire_uuid; \
        \ALTER TABLE project_comment_thread RENAME CONSTRAINT questionnaire_comment_thread_tenant_uuid TO project_comment_thread_tenant_uuid; \
        \ \
        \ALTER TABLE questionnaire_comment RENAME TO project_comment; \
        \ALTER TABLE project_comment RENAME CONSTRAINT questionnaire_comment_pk TO project_comment_pk; \
        \ALTER TABLE project_comment RENAME CONSTRAINT questionnaire_comment_comment_thread_uuid TO project_comment_comment_thread_uuid; \
        \ALTER TABLE project_comment RENAME CONSTRAINT questionnaire_comment_tenant_uuid TO project_comment_tenant_uuid; \
        \ \
        \ALTER TABLE config_questionnaire RENAME TO config_project; \
        \ALTER TABLE config_project RENAME CONSTRAINT config_questionnaire_pk TO config_project_pk; \
        \ALTER TABLE config_project RENAME CONSTRAINT config_questionnaire_tenant_uuid_fk TO config_project_tenant_uuid_fk; \
        \ \
        \ALTER TABLE document RENAME COLUMN questionnaire_uuid TO project_uuid; \
        \ALTER TABLE document RENAME COLUMN questionnaire_event_uuid TO project_event_uuid; \
        \ALTER TABLE document RENAME COLUMN questionnaire_replies_hash TO project_replies_hash; \
        \ALTER TABLE document RENAME CONSTRAINT document_questionnaire_uuid_fk TO document_project_uuid_fk; \
        \ \
        \ALTER TABLE document_template_draft_data RENAME COLUMN questionnaire_uuid TO project_uuid; \
        \ALTER TABLE document_template_draft_data RENAME CONSTRAINT document_template_draft_data_questionnaire_uuid_fk TO document_template_draft_data_project_uuid_fk; \
        \ \
        \ALTER TABLE tenant_limit_bundle RENAME COLUMN questionnaires TO projects; \
        \ \
        \UPDATE config_project SET visibility_default_value = 'PrivateProjectVisibility' WHERE visibility_default_value = 'PrivateQuestionnaire'; \
        \UPDATE config_project SET visibility_default_value = 'VisibleViewProjectVisibility' WHERE visibility_default_value = 'VisibleViewQuestionnaire'; \
        \UPDATE config_project SET visibility_default_value = 'VisibleCommentProjectVisibility' WHERE visibility_default_value = 'VisibleCommentQuestionnaire'; \
        \UPDATE config_project SET visibility_default_value = 'VisibleEditProjectVisibility' WHERE visibility_default_value = 'VisibleEditQuestionnaire'; \
        \ \
        \UPDATE config_project SET sharing_default_value = 'RestrictedProjectSharing' WHERE sharing_default_value = 'RestrictedQuestionnaire'; \
        \UPDATE config_project SET sharing_default_value = 'AnyoneWithLinkViewProjectSharing' WHERE sharing_default_value = 'AnyoneWithLinkViewQuestionnaire'; \
        \UPDATE config_project SET sharing_default_value = 'AnyoneWithLinkCommentProjectSharing' WHERE sharing_default_value = 'AnyoneWithLinkCommentQuestionnaire'; \
        \UPDATE config_project SET sharing_default_value = 'AnyoneWithLinkEditProjectSharing' WHERE sharing_default_value = 'AnyoneWithLinkEditQuestionnaire'; \
        \ \
        \UPDATE config_project SET creation = 'CustomProjectCreation' WHERE creation = 'CustomQuestionnaireCreation'; \
        \UPDATE config_project SET creation = 'TemplateProjectCreation' WHERE creation = 'TemplateQuestionnaireCreation'; \
        \UPDATE config_project SET creation = 'TemplateAndCustomProjectCreation' WHERE creation = 'TemplateAndCustomQuestionnaireCreation'; \
        \ \
        \UPDATE user_entity set permissions = replace(permissions::varchar, 'QTN_PERM', 'PRJ_PERM')::varchar[]; \
        \UPDATE user_entity set permissions = replace(permissions::varchar, 'QTN_IMPORTER_PERM', 'PRJ_IMPORTER_PERM')::varchar[]; \
        \UPDATE user_entity set permissions = replace(permissions::varchar, 'QTN_TML_PERM', 'PRJ_TML_PERM')::varchar[]; \
        \UPDATE user_entity set permissions = replace(permissions::varchar, 'QTN_ACTION_PERM', 'PRJ_ACTION_PERM')::varchar[]; \
        \UPDATE user_entity set permissions = replace(permissions::varchar, 'QTN_FILE_PERM', 'PRJ_FILE_PERM')::varchar[]; \
        \ \
        \UPDATE project SET visibility = 'PrivateProjectVisibility' WHERE visibility = 'PrivateQuestionnaire'; \
        \UPDATE project SET visibility = 'VisibleViewProjectVisibility' WHERE visibility = 'VisibleViewQuestionnaire'; \
        \UPDATE project SET visibility = 'VisibleCommentProjectVisibility' WHERE visibility = 'VisibleCommentQuestionnaire'; \
        \UPDATE project SET visibility = 'VisibleEditProjectVisibility' WHERE visibility = 'VisibleEditQuestionnaire'; \
        \ \
        \UPDATE project SET sharing = 'RestrictedProjectSharing' WHERE sharing = 'RestrictedQuestionnaire'; \
        \UPDATE project SET sharing = 'AnyoneWithLinkViewProjectSharing' WHERE sharing = 'AnyoneWithLinkViewQuestionnaire'; \
        \UPDATE project SET sharing = 'AnyoneWithLinkCommentProjectSharing' WHERE sharing = 'AnyoneWithLinkCommentQuestionnaire'; \
        \UPDATE project SET sharing = 'AnyoneWithLinkEditProjectSharing' WHERE sharing = 'AnyoneWithLinkEditQuestionnaire'; \
        \ \
        \ALTER TYPE questionnaire_event_type RENAME TO project_event_type; \
        \ \
        \UPDATE user_entity set permissions = replace(permissions::varchar, 'QTN_PERM', 'PRJ_PERM')::varchar[]; \
        \UPDATE user_entity set permissions = replace(permissions::varchar, 'QTN_IMPORTER_PERM', 'PRJ_IMPORTER_PERM')::varchar[]; \
        \UPDATE user_entity set permissions = replace(permissions::varchar, 'QTN_TML_PERM', 'PRJ_TML_PERM')::varchar[]; \
        \UPDATE user_entity set permissions = replace(permissions::varchar, 'QTN_ACTION_PERM', 'PRJ_ACTION_PERM')::varchar[]; \
        \UPDATE user_entity set permissions = replace(permissions::varchar, 'QTN_FILE_PERM', 'PRJ_FILE_PERM')::varchar[];"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

renameAuditLogsAndPersistentCommands dbPool = do
  let sql =
        "UPDATE persistent_command SET component = 'project', function  = 'createProjects' WHERE component = 'questionnaire' AND function = 'createQuestionnaires'; \
        \UPDATE persistent_command SET component = 'project_file' WHERE component = 'questionnaire_file'; \
        \ \
        \UPDATE audit SET component = 'tenant_config' WHERE component = 'app_config'; \
        \UPDATE audit SET component = 'knowledge_model_editor' WHERE component = 'branch'; \
        \UPDATE audit SET component = 'knowledge_model_migration' WHERE component = 'knowledge_model.migration'; \
        \UPDATE audit SET component = 'knowledge_model_package' WHERE component = 'package'; \
        \UPDATE audit SET component = 'knowledge_model_bundle' WHERE component = 'package_bundle'; \
        \UPDATE audit SET component = 'project' WHERE component = 'questionnaire'; \
        \UPDATE audit SET component = 'project_importer' WHERE component = 'questionnaireImporter'; \
        \UPDATE audit SET component = 'project_migration' WHERE component = 'questionnaire.migration'; \
        \UPDATE audit SET component = 'document_template_bundle' WHERE component = 'template_bundle'; \
        \UPDATE audit SET action = 'createByAdmin' WHERE action = 'create_by_admin';"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
