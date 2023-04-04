module Wizard.Localization.Messages.Public where

import Data.Maybe (fromMaybe)

import Shared.Model.Localization.LocaleRecord

-- --------------------------------------
-- VALIDATION
-- --------------------------------------
-- Uniqueness
_ERROR_VALIDATION__APP_ID_UNIQUENESS =
  LocaleRecord "error.validation.app_id_uniqueness" "Provided appId is already used" []

_ERROR_VALIDATION__DOC_TML_FILE_OR_ASSET_UNIQUENESS =
  LocaleRecord
    "error.validation.doc_tml_file_or_asset_uniqueness"
    "DocumentTemplate file or asset must have unique filename across the template"
    []

_ERROR_VALIDATION__KM_MIGRATION_UNIQUENESS =
  LocaleRecord "error.validation.km_migration_uniqueness" "Migration of Knowledge Model already exists" []

_ERROR_VALIDATION__LCL_ID_UNIQUENESS lclId =
  LocaleRecord "error.validation.lcl_id_uniqueness" "Locale '%s' already exists" [lclId]

-- Absence
_ERROR_VALIDATION__APP_ABSENCE host = LocaleRecord "error.validation.app_absence" "App ('%s') doesn't exist" [host]

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

_ERROR_VALIDATION__OPENID_WRONG_RESPONSE error =
  LocaleRecord "error.validation.openid_wrong_response" "There was a wrong response from OpenID (error: '%s')" [error]

_ERROR_VALIDATION__OPENID_CODE_ABSENCE =
  LocaleRecord "error.validation.openid_code_absence" "Auth Code is not provided" []

_ERROR_VALIDATION__OPENID_PROFILE_INFO_ABSENCE =
  LocaleRecord "error.validation.openid_profile_info_absence" "Profile Information from OpenID service is missing" []

_ERROR_VALIDATION__SUBMISSION_DEFINITION_ABSENCE subId =
  LocaleRecord "error.validation.submission_definition_absence" "Definition of submission ('%s') is missing" [subId]

-- Delete
_ERROR_VALIDATION__TML_CANT_BE_DELETED_BECAUSE_IT_IS_USED_BY_SOME_OTHER_ENTITY tmlId target =
  LocaleRecord "error.validation.tml_deletation" "DocumentTemplate '%s' can't be deleted. It's used by some %s" [tmlId, target]

_ERROR_VALIDATION__DEFAULT_LOCALE_DELETATION =
  LocaleRecord "error.validation.default_locale_deletation" "You can't delete default locale" []

_ERROR_VALIDATION__DEFAULT_WIZARD_LOCALE_DELETATION =
  LocaleRecord "error.validation.default_locale_deletation" "You can't delete default wizard locale" []

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

-- Locale
_ERROR_VALIDATION__DEACTIVATE_DEFAULT_LOCALE =
  LocaleRecord "error.validation.deactivate_default_locale" "You can't deactivate default locale" []

_ERROR_VALIDATION__LOCALE_DISABLED_DEFAULT =
  LocaleRecord "error.validation.locale_disabled_default" "You can't set disabled locale as default" []

-- --------------------------------------
-- SERVICE
-- --------------------------------------
-- Common
_ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED featureName =
  LocaleRecord "error.service.common.feature_is_disabled" "Feature '%s' is disabled" [featureName]

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

_ERROR_SERVICE_QTN__UNABLE_TO_GENERATE_DOCUMENT_PREVIEW workerLog =
  LocaleRecord "error.service.qtn.unable_to_generate_document_preview" "%s" [fromMaybe "no log provided" workerLog]

-- DocumentTemplate Bundle
_ERROR_SERVICE_TB__PULL_NON_EXISTING_TML tmlId =
  LocaleRecord "error.service.tb.pull_non_existing_tml" "Desired template ('%s') wasn't found in Registry" [tmlId]

-- Token
_ERROR_SERVICE_TOKEN__INCORRECT_EMAIL_OR_PASSWORD =
  LocaleRecord "error.service.token.incorrect_email_or_password" "Incorrect email or password" []

_ERROR_SERVICE_TOKEN__INCORRECT_CODE =
  LocaleRecord "error.service.token.incorrect_code" "Incorrect code" []

_ERROR_SERVICE_TOKEN__ACCOUNT_IS_NOT_ACTIVATED =
  LocaleRecord "error.service.token.account_is_not_activated" "The account is not activated" []

_ERROR_SERVICE_TOKEN__UNABLE_TO_DECODE_AND_VERIFY_TOKEN =
  LocaleRecord "error.service.token.unable_to_decode_token" "Unable to decode and verify token" []

_ERROR_SERVICE_TOKEN__OBSOLETE_TOKEN_VERSION =
  LocaleRecord "error.service.token.obsolete_token_version" "Obsolete token version" []

_ERROR_SERVICE_TOKEN__UNABLE_TO_GET_TOKEN_VERSION =
  LocaleRecord "error.service.token.unable_to_get_token_version" "Unable to get token version from token" []

_ERROR_SERVICE_TOKEN__CODE_IS_EXPIRED = LocaleRecord "error.service.token.code_expired" "Code is expired" []

_ERROR_SERVICE_TOKEN__TOKEN_IS_EXPIRED = LocaleRecord "error.service.token.token_expired" "Token is expired" []

_ERROR_SERVICE_TOKEN__UNKNOWN_TECHNICAL_DIFFICULTIES =
  LocaleRecord "error.service.token.unknown_technical_difficulties" "Unknown technical difficulties" []

_ERROR_SERVICE_TOKEN__UNABLE_TO_GET_TOKEN_EXPIRATION =
  LocaleRecord "error.service.token.unable_to_get_token_expiration" "Unable to get token expiration from token" []

-- Typehint
_ERROR_SERVICE_TYPEHINT__BAD_TYPE_OF_QUESTION =
  LocaleRecord "error.service.typehint.bad_type_of_question" "Desired question has to be integration question" []

_ERROR_SERVICE_TYPEHINT__BAD_TYPE_OF_INTEGRATION =
  LocaleRecord "error.service.typehint.bad_type_of_integration" "Desired integration has to be API integration" []

_ERROR_SERVICE_TYPEHINT__INTEGRATION_RETURNS_ERROR =
  LocaleRecord "error.service.typehint.integration_request_failed" "Integration request failed" []

-- User
_ERROR_SERVICE_USER__MISSING_USER =
  LocaleRecord "error.service.user.missing_user" "You have to be log in to perform this request." []
