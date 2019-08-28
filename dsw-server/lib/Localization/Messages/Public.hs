module Localization.Messages.Public where

import Model.Localization.LocaleRecord

-- --------------------------------------
-- API
-- --------------------------------------
_ERROR_API_COMMON__CANT_DESERIALIZE_OBJ =
  LocaleRecord "error.api.common.cant_deserialize_obj" "Problem in deserialization of JSON" []

-- --------------------------------------
-- DATABASE
-- --------------------------------------
_ERROR_DATABASE__ENTITY_NOT_FOUND entityName identificator =
  LocaleRecord "error.database.entity_not_found" "Object '%s' of type '%s' does not exist" [identificator, entityName]

-- --------------------------------------
-- VALIDATION
-- --------------------------------------
-- Format
_ERROR_VALIDATION__INVALID_KM_ID_FORMAT =
  LocaleRecord "error.validation.invalid_km_id_format" "KmId is not in the valid format" []

_ERROR_VALIDATION__INVALID_ORG_ID_FORMAT =
  LocaleRecord "error.validation.invalid_org_id_format" "OrganizationId is not in the valid format" []

_ERROR_VALIDATION__INVALID_PKG_VERSION_FORMAT =
  LocaleRecord "error.validation.invalid_pkg_id_format" "Version is not in the valid format" []

_ERROR_VALIDATION__UNSUPPORTED_DMP_FORMAT format =
  LocaleRecord "error.validation.unsupported_dmp_format" "This Document format (%s) is not currently supported" [format]

-- Uniqueness
_ERROR_VALIDATION__KM_ID_UNIQUENESS kmId =
  LocaleRecord "error.validation.km_id_uniqueness" "KmId '%s' already exists" [kmId]

_ERROR_VALIDATION__KM_MIGRATION_UNIQUENESS =
  LocaleRecord "error.validation.km_migration_uniqueness" "Migration of Knowledge Model already exists" []

_ERROR_VALIDATION__PKG_ID_UNIQUENESS pkgId =
  LocaleRecord "error.validation.pkg_id_uniqueness" "Package '%s' already exists" [pkgId]

_ERROR_VALIDATION__USER_EMAIL_UNIQUENESS email =
  LocaleRecord "error.validation.user_email_uniqueness" "User (email: '%s') already exists" [email]

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

-- Security
_ERROR_VALIDATION__FORBIDDEN action = LocaleRecord "error.validation.forbidden" "Forbidden to perform '%s'" [action]

-- --------------------------------------
-- SERVICE
-- --------------------------------------
-- Common
_ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED featureName =
  LocaleRecord "error.service.common.feature_is_disabled" "Feature '%s' is disabled" [featureName]

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

_ERROR_SERVICE_PKG__HIGHER_NUMBER_IN_NEW_VERSION =
  LocaleRecord
    "error.service.pkg.highest_number_in_new_version"
    "Your new version has to be higher than the previous version"
    []

_ERROR_SERVICE_PKG__PKG_CANT_BE_DELETED_BECAUSE_IT_IS_USED_BY_SOME_OTHER_ENTITY pkgId target =
  LocaleRecord
    "error.service.pkg.pkg_cant_be_deleted_because_it_is_used_by_some_other_entity"
    "Knowledge model ('%s') can't be deleted because some %s depends on it"
    [pkgId, target]

_ERROR_SERVICE_PKG__PKG_ID_MISMATCH pkgId =
  LocaleRecord
    "error.service.pkg.pkg_id_mismatch"
    "Knowledge model ID '%s' doesn't correspond with Knowledge model coordinates"
    [pkgId]

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

-- Token
_ERROR_SERVICE_TOKEN__INCORRECT_EMAIL_OR_PASSWORD =
  LocaleRecord "error.service.token.Incorrect_email_or_password" "Incorrect email or password" []

_ERROR_SERVICE_TOKEN__ACCOUNT_IS_NOT_ACTIVATED =
  LocaleRecord "error.service.token.account_is_not_activated" "The account is not activated" []

-- Template
_ERROR_SERVICE_TYPEHINT__BAD_TYPE_OF_QUESTION =
  LocaleRecord "error.service.typehint.bad_type_of_question" "Desired question has to be integration question" []

-- User
_ERROR_SERVICE_USER__REQUIRED_ADMIN_ROLE_OR_HASH_IN_QUERY_PARAMS =
  LocaleRecord
    "error_service.user.required_admin_role_or_hash_in_query_params"
    "You have to log in as an Administrator or you have to provide a hash in the query param"
    []

_ERROR_SERVICE_USER__REQUIRED_HASH_IN_QUERY_PARAMS =
  LocaleRecord "error_service.user.required_hash_in_query_params" "A hash query param has to be provided" []

_ERROR_SERVICE_USER__MISSING_USER =
  LocaleRecord "error_service.user.missing_user" "You have to be log in to perform this request." []

-- --------------------------------------
-- UTIL
-- --------------------------------------
-- JSON
_ERROR_UTIL_JSON__VALUE_IS_NOT_OBJECT =
  LocaleRecord
    "error.util.json.value_is_not_object"
    "Problem in deserialization of JSON (reason: provided value is not an object)"
    []

_ERROR_UTIL_JSON__MISSING_FIELD_IN_OBJECT fieldName =
  LocaleRecord
    "error.util.json.missing_field_in_object"
    "Problem in deserialization of JSON (reason: missing '%s' key in provided object)"
    [fieldName]

_ERROR_UTIL_JSON__BAD_FIELD_TYPE fieldName fieldType =
  LocaleRecord
    "error.util.json.bad_field_type"
    "Problem in deserialization of JSON (reason: type of '%s' in provided object is not '%s')"
    [fieldName, fieldType]

_ERROR_UTIL_JSON__CANT_DESERIALIZE_FIELD fieldName error =
  LocaleRecord
    "error.util.json.cant_deserialize_field"
    "Problem in deserialization of JSON (field: '%s', error: %s)"
    [fieldName, error]
