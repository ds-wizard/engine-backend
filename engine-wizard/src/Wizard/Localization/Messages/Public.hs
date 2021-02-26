module Wizard.Localization.Messages.Public where

import Shared.Model.Localization.LocaleRecord

-- --------------------------------------
-- VALIDATION
-- --------------------------------------
-- General
_ERROR_VALIDATION__COORDINATE_MISMATCH coordinate =
  LocaleRecord
    "error.validation.coordinate_mismatch"
    "Coordinate '%s' doesn't correspond with 'orgId', 'kmId/templateId' or 'version'"
    [coordinate]

-- Format
_ERROR_VALIDATION__INVALID_COORDINATE_FORMAT =
  LocaleRecord "error.validation.invalid_coordinate_format" "Coordinate is not in the valid format" []

_ERROR_VALIDATION__INVALID_COORDINATE_VERSION_FORMAT =
  LocaleRecord "error.validation.invalid_coordinate_version_format" "Version is not in the valid format" []

-- Uniqueness
_ERROR_VALIDATION__KM_MIGRATION_UNIQUENESS =
  LocaleRecord "error.validation.km_migration_uniqueness" "Migration of Knowledge Model already exists" []

-- Absence
_ERROR_VALIDATION__BRANCH_PREVIOUS_PKG_ABSENCE =
  LocaleRecord "error.validation.branch_previous_pkg_absence" "Branch has to be based on a package" []

_ERROR_VALIDATION__INTEGRATION_ABSENCE =
  LocaleRecord "error.validation.integration_absence" "Desired integrations doesn't exist" []

_ERROR_VALIDATION__MAIN_PKG_OF_PB_ABSENCE =
  LocaleRecord "error.validation.main_pkg_of_pb_absence" "Package Bundle doesn't contain main package" []

_ERROR_VALIDATION__PREVIOUS_PKG_ABSENCE =
  LocaleRecord "error.validation.previous_pkg_absence" "Previous package doesn't exist" []

_ERROR_VALIDATION__QUESTION_ABSENCE =
  LocaleRecord "error.validation.question_absence" "Desired question doesn't exist" []

_ERROR_VALIDATION__TEMPLATE_ABSENCE = LocaleRecord "error.validation.template_absence" "Template doesn't exist" []

_ERROR_VALIDATION__USER_ABSENCE userUuid =
  LocaleRecord "error.validation.user_absence" "User ('%s') doesn't exist" [userUuid]

_ERROR_VALIDATION__OPENID_CODE_ABSENCE =
  LocaleRecord "error.validation.openid_code_absence" "Auth Code is not provided" []

_ERROR_VALIDATION__OPENID_PROFILE_INFO_ABSENCE =
  LocaleRecord "error.validation.openid_profile_info_absence" "Profile Information from OpenID service is missing" []

_ERROR_VALIDATION__SUBMISSION_DEFINITION_ABSENCE subId =
  LocaleRecord "error.validation.submission_definition_absence" "Definition of submission ('%s') is missing" [subId]

-- Delete
_ERROR_VALIDATION__TML_CANT_BE_DELETED_BECAUSE_IT_IS_USED_BY_SOME_OTHER_ENTITY tmlId target =
  LocaleRecord "error.validation.tml_deletation" "Template '%s' can't be deleted. It's used by some %s" [tmlId, target]

-- Unsupported version
_ERROR_VALIDATION__TEMPLATE_UNSUPPORTED_VERSION tmlId tmlMetamodelVersion appTmlMetamodelVersion =
  LocaleRecord
    "error.validation.tml_unsupported_version"
    "Template '%s' contains unsupported version of metamodel (template metamodel version: '%s', application metamodel version: '%s')"
    [tmlId, show tmlMetamodelVersion, show appTmlMetamodelVersion]

-- --------------------------------------
-- SERVICE
-- --------------------------------------
-- Common
_ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED featureName =
  LocaleRecord "error.service.common.feature_is_disabled" "Feature '%s' is disabled" [featureName]

_ERROR_SERVICE_DOCUMENT__TEMPLATE_OR_FORMAT_NOT_SET_UP =
  LocaleRecord "error.service.template.template_or_format_not_set_up" "Template or format is not set up" []

-- Auth
_ERROR_SERVICE_AUTH__SERVICE_NOT_DEFINED authId =
  LocaleRecord "error.service.auth.service_not_defined" "Service '%s' is not defined" [authId]

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

_ERROR_SERVICE_QTN__UNABLE_TO_GENERATE_DOCUMENT_PREVIEW =
  LocaleRecord "error.service.qtn.unable_to_generate_document_preview" "Unable to generate preview" []

-- Template Bundle
_ERROR_SERVICE_TB__PULL_NON_EXISTING_TML tmlId =
  LocaleRecord "error.service.tb.pull_non_existing_tml" "Desired template ('%s') wasn't found in Registry" [tmlId]

-- Token
_ERROR_SERVICE_TOKEN__INCORRECT_EMAIL_OR_PASSWORD =
  LocaleRecord "error.service.token.Incorrect_email_or_password" "Incorrect email or password" []

_ERROR_SERVICE_TOKEN__ACCOUNT_IS_NOT_ACTIVATED =
  LocaleRecord "error.service.token.account_is_not_activated" "The account is not activated" []

_ERROR_SERVICE_TOKEN__UNABLE_TO_GET_OR_VERIFY_SERVICE_TOKEN =
  LocaleRecord "error.service.token.bad_service_token" "Unable to get or verify service token" []

-- Template
_ERROR_SERVICE_TYPEHINT__BAD_TYPE_OF_QUESTION =
  LocaleRecord "error.service.typehint.bad_type_of_question" "Desired question has to be integration question" []

-- User
_ERROR_SERVICE_USER__MISSING_USER =
  LocaleRecord "error.service.user.missing_user" "You have to be log in to perform this request." []
