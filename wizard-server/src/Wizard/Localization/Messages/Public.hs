module Wizard.Localization.Messages.Public where

import Data.Maybe (fromMaybe)

import Shared.Common.Model.Localization.LocaleRecord

-- --------------------------------------
-- VALIDATION
-- --------------------------------------
-- Uniqueness
_ERROR_VALIDATION__TENANT_ID_UNIQUENESS =
  LocaleRecord "error.validation.tenant_id_uniqueness" "Provided tenantId is already used" []

_ERROR_VALIDATION__DOC_TML_FILE_OR_ASSET_UNIQUENESS =
  LocaleRecord
    "error.validation.doc_tml_file_or_asset_uniqueness"
    "DocumentTemplate file or asset must have unique filename across the template"
    []

_ERROR_VALIDATION__KM_MIGRATION_UNIQUENESS =
  LocaleRecord "error.validation.km_migration_uniqueness" "Migration of Knowledge Model already exists" []

_ERROR_VALIDATION__USER_EMAIL_UNIQUENESS email =
  LocaleRecord "error.validation.user_email_uniqueness" "User (email: '%s') already exists" [email]

-- Absence
_ERROR_VALIDATION__TENANT_ABSENCE host = LocaleRecord "error.validation.tenant_absence" "Tenant ('%s') doesn't exist" [host]

_ERROR_VALIDATION__BRANCH_PREVIOUS_PKG_ABSENCE =
  LocaleRecord "error.validation.branch_previous_pkg_absence" "Branch has to be based on a package" []

_ERROR_VALIDATION__INTEGRATION_ABSENCE =
  LocaleRecord "error.validation.integration_absence" "Desired integrations doesn't exist" []

_ERROR_VALIDATION__PREVIOUS_PKG_ABSENCE =
  LocaleRecord "error.validation.previous_pkg_absence" "Previous package doesn't exist" []

_ERROR_VALIDATION__QUESTION_ABSENCE =
  LocaleRecord "error.validation.question_absence" "Desired question doesn't exist" []

_ERROR_VALIDATION__TEMPLATE_ABSENCE = LocaleRecord "error.validation.template_absence" "DocumentTemplate doesn't exist" []

_ERROR_VALIDATION__USER_ABSENCE userUuid =
  LocaleRecord "error.validation.user_absence" "User ('%s') doesn't exist" [userUuid]

_ERROR_VALIDATION__TOKEN_ABSENCE tokenUuid =
  LocaleRecord "error.validation.token_absence" "Token ('%s') doesn't exist" [tokenUuid]

-- Delete
_ERROR_VALIDATION__TML_CANT_BE_DELETED_BECAUSE_IT_IS_USED_BY_SOME_OTHER_ENTITY tmlId target =
  LocaleRecord "error.validation.tml_deletation" "DocumentTemplate '%s' can't be deleted. It's used by some %s" [tmlId, target]

-- Unsupported version
_ERROR_VALIDATION__PKG_UNSUPPORTED_METAMODEL_VERSION pkgMetamodelVersion appPkgMetamodelVersion =
  LocaleRecord
    "error.validation.pkg_unsupported_metamodel_version"
    "Package contains unsupported version of metamodel (pkg metamodel version: '%s', application metamodel version: '%s')"
    [show pkgMetamodelVersion, show appPkgMetamodelVersion]

_ERROR_VALIDATION__TEMPLATE_UNSUPPORTED_METAMODEL_VERSION tmlId tmlMetamodelVersion appTmlMetamodelVersion =
  LocaleRecord
    "error.validation.tml_unsupported_metamodel_version"
    "DocumentTemplate '%s' contains unsupported version of metamodel (template metamodel version: '%s', application metamodel version: '%s')"
    [tmlId, show tmlMetamodelVersion, show appTmlMetamodelVersion]

-- Unsupported State
_ERROR_VALIDATION__DOC_TML_UNSUPPORTED_STATE tmlId phase =
  LocaleRecord "error.validation.doc_tml_unsupported_state" "You can not move '%s' into the following phase: %s" [tmlId, phase]

-- Questionnaire File
_ERROR_VALIDATION__QUESTIONNAIRE_FILE_SIZE_EXCEEDS_LIMIT fileSize maxFileSize =
  LocaleRecord "error.validation.questionnaire_file_size_exceeds_limit" "File exceeds the maximum allowed size (file: %s, maximum: %s)" [show fileSize, show maxFileSize]

_ERROR_VALIDATION__QUESTIONNAIRE_FILE_QUESTION_ABSENCE_OR_WRONG_TYPE =
  LocaleRecord "error.validation.questionnaire_file_question_absence_or_wrong_type" "The question either doesn't exist or is not a File Question" []

-- --------------------------------------
-- SERVICE
-- --------------------------------------
_ERROR_SERVICE_DOCUMENT__TEMPLATE_OR_FORMAT_NOT_SET_UP =
  LocaleRecord "error.service.template.template_or_format_not_set_up" "DocumentTemplate or format is not set up" []

_ERROR_SERVICE_DOCUMENT__QUESTIONNAIRE_OR_FORMAT_NOT_SET_UP =
  LocaleRecord "error.service.template.questionnaire_or_format_not_set_up" "Questionnaire or format is not set up" []

-- Auth
_ERROR_SERVICE_AUTH__SERVICE_NOT_DEFINED authId =
  LocaleRecord "error.service.auth.service_not_defined" "Service '%s' is not defined" [authId]

-- Branch
_ERROR_SERVICE_BRANCH__KM_MIGRATION_EXISTS =
  LocaleRecord "error.service.branch.km_migration_exists" "You can't publish the branch when there is ongoing KM migration" []

-- Locale Bundle
_ERROR_SERVICE_LB__PULL_NON_EXISTING_LOCALE lclId =
  LocaleRecord "error.service.lb.pull_non_existing_locale" "Desired locale ('%s') wasn't found in Registry" [lclId]

-- Migration / KnowledgeModel
_ERROR_SERVICE_MIGRATION_KM__TARGET_PKG_IS_NOT_HIGHER =
  LocaleRecord
    "error.service.migration.km.target_pkg_is_not_higher"
    "You have to choose a knowledge model which is newer than current knowledge model"
    []

_ERROR_SERVICE_MIGRATION_KM__BRANCH_MISSING_MERGE_CHECKPOINT_PACKAGE_ID =
  LocaleRecord
    "error.service.migration.km.branch_missing_merge_checkpoint_package_id"
    "Branch is missing some metadata ('mergeCheckpointPackageId')"
    []

_ERROR_SERVICE_MIGRATION_KM__BRANCH_MISSING_FORK_OF_PACKAGE_ID =
  LocaleRecord
    "error.service.migration.km.branch_missing_fork_of_package_id"
    "Branch is missing some metadata ('forkOfPackageId')"
    []

_ERROR_SERVICE_MIGRATION_KM__NO_CONFLICTS_TO_SOLVE =
  LocaleRecord
    "error.service.migration.km.no_conflicts_to_solve"
    "You can't solve conflicts because there are no conflicts"
    []

_ERROR_SERVICE_MIGRATION_KM__NO_EVENTS_IN_TARGET_PKG_EVENT_QUEUE =
  LocaleRecord "error.service.migration.km.no_events_in_target_pkg_event_queue" "There no more events to solve" []

_ERROR_SERVICE_MIGRATION_KM__EDIT_ACTION_HAS_TO_PROVIDE_TARGET_EVENT =
  LocaleRecord
    "error.service.migration.km.edit_action_has_to_provide_target_event"
    "Migration action is missing information about currently solving event"
    []

_ERROR_SERVICE_MIGRATION_KM__EVENT_UUIDS_MISMATCH =
  LocaleRecord
    "error.service.migration.km.event_uuids_mismatch"
    "There is a problem with an event metadata (mismatch in events' uuids)"
    []

-- Package
_ERROR_SERVICE_PKG__IMPORT_PREVIOUS_PKG_AT_FIRST previousPkgId pkgId =
  LocaleRecord
    "error.service.pkg.import_previous_pkg_at_first"
    "The knowledge model ('%s') depends on '%s'. Therefore, you should import the knowledge model '%s' at first."
    [pkgId, previousPkgId, previousPkgId]

_ERROR_SERVICE_PKG__PKG_CANT_BE_DELETED_BECAUSE_IT_IS_USED_BY_SOME_OTHER_ENTITY pkgId target =
  LocaleRecord
    "error.service.pkg.pkg_cant_be_deleted_because_it_is_used_by_some_other_entity"
    "Knowledge model ('%s') can't be deleted because some %s depends on it"
    [pkgId, target]

-- Package Bundle
_ERROR_SERVICE_PB__PULL_NON_EXISTING_PKG pkgId =
  LocaleRecord
    "error.service.pb.pull_non_existing_pkg"
    "Desired knowledge model ('%s') wasn't found in Registry"
    [pkgId]

-- Questionnaire
_ERROR_SERVICE_QTN__QTN_CANT_BE_DELETED_BECAUSE_IT_IS_USED_IN_MIGRATION =
  LocaleRecord
    "error.service.qtn.qtn_cant_be_deleted_because_it_is_used_in_migration"
    "Questionnaire can't be deleted because it's used in some questionnaire migration"
    []

_ERROR_SERVICE_QTN_COLLABORATION__FORCE_DISCONNECT qtnUuid =
  LocaleRecord
    "error.service.qtn.collaboration.force_disconnect"
    "Questionnaire ('%s') dramatically changed its state. Therefore, users has to be disconnected"
    [qtnUuid]

_ERROR_SERVICE_QTN_VERSION__NON_EXISTENT_EVENT_UUID eventUuid =
  LocaleRecord
    "error.service.qtn.version.non_existent_event_uuid"
    "You can't create version for non-existent event (eventUuid: '%s')"
    [eventUuid]

_ERROR_SERVICE_QTN_VERSION__VERSION_UNIQUENESS eventUuid =
  LocaleRecord
    "error.service.qtn.version.version_uniqueness"
    "There is already a version for the event (eventUuid: '%s')"
    [eventUuid]

_ERROR_SERVICE_QTN__UNABLE_TO_GENERATE_DOCUMENT_PREVIEW workerLog =
  LocaleRecord "error.service.qtn.unable_to_generate_document_preview" "%s" [fromMaybe "no log provided" workerLog]

-- DocumentTemplate Bundle
_ERROR_SERVICE_TB__PULL_NON_EXISTING_TML tmlId =
  LocaleRecord "error.service.tb.pull_non_existing_tml" "Desired template ('%s') wasn't found in Registry" [tmlId]

-- Typehint
_ERROR_SERVICE_TYPEHINT__BAD_TYPE_OF_QUESTION =
  LocaleRecord "error.service.typehint.bad_type_of_question" "Desired question has to be integration question" []

_ERROR_SERVICE_TYPEHINT__BAD_TYPE_OF_INTEGRATION =
  LocaleRecord "error.service.typehint.bad_type_of_integration" "Desired integration has to be API integration" []

_ERROR_SERVICE_TYPEHINT__INTEGRATION_RETURNS_ERROR =
  LocaleRecord "error.service.typehint.integration_request_failed" "Integration request failed" []
