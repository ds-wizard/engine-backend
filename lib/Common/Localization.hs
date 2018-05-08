module Common.Localization where

-- --------------------------------------
-- DATABASE
-- --------------------------------------
_ERROR_DATABASE__DESERIALIZATION_FAILED = "Problem with deserialization of entity from database"

_ERROR_DATABASE__ENTITY_NOT_FOUND = "Entity does not exist"

-- --------------------------------------
-- VALIDATION
-- --------------------------------------
-- Form
_ERROR_VALIDATION__INVALID_ARTIFACT_FORMAT = "ArtifactId is not in valid format"

_ERROR_VALIDATION__INVALID_PKG_VERSION_FORMAT = "Version is not in valid format"

_ERROR_VALIDATION__INVALID_GROUPID_FORMAT = "GroupId is not in valid format"

-- Uniqueness
_ERROR_VALIDATION__USER_EMAIL_UNIQUENESS email = "User with email '" ++ email ++ "' already exists"

_ERROR_VALIDATION__ARTIFACT_ID_UNIQUENESS artifactId = "ArtifactId '" ++ artifactId ++ "' is already taken"

_ERROR_VALIDATION__PKG_ID_UNIQUENESS pkgId = "Package '" ++ pkgId ++ "' already exists"

-- Absence
_ERROR_VALIDATION__KM_ABSENCE = "Knowledge Model does not exist"

_ERROR_VALIDATION__PARENT_PKG_ABSENCE = "Parent package doesn't exist"

-- --------------------------------------
-- SERVICE
-- --------------------------------------
-- Package
_ERROR_SERVICE_PKG__IMPORT_PARENT_PKG_AT_FIRST parentPkgId pkgId =
  "Parent ('" ++ parentPkgId ++ "') of imported package ('" ++ pkgId ++ "') is missing. Please import parent at first"

_ERROR_SERVICE_PKG__HIGHER_NUMBER_IN_NEW_VERSION = "New version has to be higher than the previous one"

_ERROR_SERVICE_PKG__PKG_CANT_BE_DELETED_BECAUSE_IT_IS_USED_BY_SOME_OTHER_PKG pkgId target =
  "Package '" ++ pkgId ++ "' can't be deleted. It's used by some " ++ target ++ "."

-- Token
_ERROR_SERVICE_TOKEN__INCORRECT_EMAIL_OR_PASSWORD = "Incorrect email or password"

_ERROR_SERVICE_TOKEN__ACCOUNT_IS_NOT_ACTIVATED = "Account is not activated"

-- User
_ERROR_SERVICE_USER__REQUIRED_ADMIN_ROLE_OR_HASH_IN_QUERY_PARAMS =
  "You have to log in as Administrator or you have to provide a hash in query param"

_ERROR_SERVICE_USER__REQUIRED_HASH_IN_QUERY_PARAMS = "Hash query param has to be provided"

-- --------------------------------------
-- MIGRATION TOOL
-- --------------------------------------
-- --------------
-- 1. Applicator
-- --------------
_ERROR_MT_APPLICATOR__UNSPECIFIED_ERROR = "Unspecified problem in building Knowledge Model happened"

_ERROR_MT_APPLICATOR__CREATE_KM_AT_FIRST = "You have to create Knowledge Model at first"

_ERROR_MT_APPLICATOR__BAD_APPLICATION eventName entityName = "You can't apply " ++ eventName ++ " to " ++ entityName

_ERROR_MT_APPLICATOR__Q_TYPE_LIST_REQUIRES_AIT = "Event question type 'list' should have answerItemTemplate filled"

_ERROR_MT_APPLICATOR__YOU_CANT_ADD_QUESTION_TO_NON_EXISTING_AIT =
  "You can't add question non-existing AnswerItemTemplate"

-- Uniqueness
_ERROR_MT_VALIDATION_APPLICATOR__KM_UNIQUENESS = "Knowledge Model is already created"

-- ------------
-- 2. Migrator
-- ------------
_ERROR_MT_MIGRATOR__TARGET_PKG_IS_NOT_HIGHER = "Target Package is not higher than current one"

_ERROR_MT_MIGRATOR__BRANCH_HAS_TO_HAVE_MERGE_CHECKPOINT = "Branch has to have merge checkpoint"

_ERROR_MT_MIGRATOR__BRANCH_HAS_TO_HAVE_CHECKPOINT_ABOUT_LAST_MERGED_PARENT_PKG =
  "Branch has to have checkpoint what was last parent package which was merged in"

_ERROR_MT_MIGRATOR__NO_CONFLICTS_TO_SOLVE = "You can't solve conflicts because Migration state isn't in conflict state"

_ERROR_MT_MIGRATOR__NO_EVENTS_IN_TARGET_PKG_EVENT_QUEUE = "No events in target package event queue"

_ERROR_MT_MIGRATOR__EDIT_ACTION_HAS_TO_PROVIDE_TARGET_EVENT = "Edit migration action has to provide target event"

_ERROR_MT_MIGRATOR__ORIGINAL_EVENT_UUID_DOES_NOT_MARCH_WITH_CURRENT_TARGET_EVENT =
  "OriginalEventUuid doesn't match with current target event"

-- Absence
_ERROR_MT_VALIDATION_MIGRATOR__SOURCE_BRANCH_ABSENCE = "Source branch does not exist"

_ERROR_MT_VALIDATION_MIGRATOR__BRANCH_PARENT_ABSENCE = "Branch has to have a parent"

_ERROR_MT_VALIDATION_MIGRATOR__TARGET_PARENT_PKG_ABSENCE = "Target parent package doesn’t exist"

-- Uniqueness
_ERROR_MT_VALIDATION_MIGRATOR__MIGRATION_UNIQUENESS = "Migration is already created"
