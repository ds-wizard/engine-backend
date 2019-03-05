module Localization where

import qualified Data.UUID as U

import Model.Event.EventPath

-- --------------------------------------
-- DATABASE
-- --------------------------------------
_ERROR_DATABASE__DESERIALIZATION_FAILED = "Problem with deserialization of entity from database"

_ERROR_DATABASE__ENTITY_NOT_FOUND = "Entity does not exist"

-- --------------------------------------
-- VALIDATION
-- --------------------------------------
-- Form
_ERROR_VALIDATION__INVALID_KM_ID_FORMAT = "KmId is not in the valid format"

_ERROR_VALIDATION__INVALID_PKG_VERSION_FORMAT = "Version is not in the valid format"

_ERROR_VALIDATION__INVALID_GROUPID_FORMAT = "OrganizationId is not in the valid format"

_ERROR_VALIDATION__UNSUPPORTED_DMP_FORMAT format =
  "This Data Management Plan format (" ++ format ++ ") is not currently supported"

-- Uniqueness
_ERROR_VALIDATION__USER_EMAIL_UNIQUENESS email = "User with email '" ++ email ++ "' already exists"

_ERROR_VALIDATION__KM_ID_UNIQUENESS kmId = "KmId '" ++ kmId ++ "' is already taken"

_ERROR_VALIDATION__PKG_ID_UNIQUENESS pkgId = "Package '" ++ pkgId ++ "' already exists"

-- Absence
_ERROR_VALIDATION__KM_ABSENCE = "Knowledge Model does not exist"

_ERROR_VALIDATION__PARENT_PKG_ABSENCE = "Parent package doesn't exist"

-- --------------------------------------
-- SERVICE
-- --------------------------------------
-- Knowledge Model Bundle
_ERROR_SERVICE_KMB__MAIN_PKG_ABSENCE = "Knowledge Model Bundle doesn't contain main package"

-- Mail
_ERROR_SERVICE_MAIL__AUTH_ERROR_MESSAGE = "Could not authenticate with SMTP server"

_ERROR_SERVICE_MAIL__EMAIL_SENT_OK recipients = "Email has been sent to: " ++ unwords recipients

_ERROR_SERVICE_MAIL__EMAIL_SENT_FAIL errorMessage = "Failed to send email: " ++ errorMessage

_ERROR_SERVICE_MAIL__MISSING_HTML mailName = "Could not load HTML template for: " ++ mailName

_ERROR_SERVICE_MAIL__MISSING_PLAIN mailName = "Could not load plain text template for: " ++ mailName

_ERROR_SERVICE_MAIL__MISSING_HTML_PLAIN mailName = "Could not load HTML nor plain text template for: " ++ mailName

-- Package
_ERROR_SERVICE_PKG__IMPORT_PARENT_PKG_AT_FIRST parentPkgId pkgId =
  "The parent ('" ++
  parentPkgId ++ "') of imported package ('" ++ pkgId ++ "') is missing. Please import the parent first."

_ERROR_SERVICE_PKG__HIGHER_NUMBER_IN_NEW_VERSION = "A new version has to be higher than the previous one"

_ERROR_SERVICE_PKG__PKG_CANT_BE_DELETED_BECAUSE_IT_IS_USED_BY_SOME_OTHER_ENTITY pkgId target =
  "Package '" ++ pkgId ++ "' can't be deleted. It's used by some " ++ target ++ "."

_ERROR_SERVICE_PKG__PKG_ID_MISMATCH pkgId = "Package ID '" ++ pkgId ++ "' doesn't correspond with Package Coordinates"

-- Public Questionnaire
_ERROR_SERVICE_PQ__NOT_SET_UP = "Public questionnaire is not set up"

-- Template
_ERROR_SERVICE_TEMPLATE__TRANSFORMATION_FAILED err = "Couldn't transform to desired document format: " ++ err

_ERROR_SERVICE_TEMPLATE__UKNOWN_FORMAT = "Unprocessable DMP format"

_ERROR_SERVICE_TEMPLATE__LOADING_TEMPLATE_FAILED reason = "Couldn't load a template from file (" ++ reason ++ ")"

-- Token
_ERROR_SERVICE_TOKEN__INCORRECT_EMAIL_OR_PASSWORD = "Incorrect email or password"

_ERROR_SERVICE_TOKEN__ACCOUNT_IS_NOT_ACTIVATED = "The account is not activated"

_ERROR_SERVICE_TOKEN__UNABLE_TO_GET_TOKEN = "Unable to get token"

_ERROR_SERVICE_TOKEN__UNABLE_TO_DECODE_AND_VERIFY_TOKEN = "Unable to decode and verify token"

_ERROR_SERVICE_TOKEN__UNABLE_TO_GET_TOKEN_VERSION = "Unable to get token version from token"

_ERROR_SERVICE_TOKEN__OBSOLETE_TOKEN_VERSION = "Obsolete token version"

_ERROR_SERVICE_TOKEN__UNABLE_TO_GET_TOKEN_EXPIRATION = "Unable to get token expiration from token"

_ERROR_SERVICE_TOKEN__TOKEN_IS_EXPIRED = "Token is expired"

_ERROR_SERVICE_TOKEN__UNKNOWN_TECHNICAL_DIFFICULTIES = "Unknown technical difficulties"

_ERROR_SERVICE_TOKEN__UNABLE_TO_GET_OR_VERIFY_SEVICE_TOKEN = "Unable to get or verify service token"

-- User
_ERROR_SERVICE_USER__REQUIRED_ADMIN_ROLE_OR_HASH_IN_QUERY_PARAMS =
  "You have to log in as an Administrator or you have to provide a hash in the query param"

_ERROR_SERVICE_USER__REQUIRED_HASH_IN_QUERY_PARAMS = "A hash query param has to be provided"

_ERROR_SERVICE_USER__MISSING_USER = "You have to be log in to run."

-- --------------------------------------
-- HTTP CLIENT
-- --------------------------------------
_ERROR_HTTP_CLIENT__REQUEST_FAILED serviceName endpoint =
  "Request to external API failed (service: '" ++ serviceName ++ "', endpoint: '" ++ endpoint ++ "')"

-- --------------------------------------
-- MIGRATION TOOL
-- --------------------------------------
-- --------------
-- 1. Applicator
-- --------------
_ERROR_MT_APPLICATOR__UNSPECIFIED_ERROR = "Unspecified problem in building Knowledge Model happened"

_ERROR_MT_APPLICATOR__CREATE_KM_AT_FIRST = "You have to create a Knowledge Model first"

_ERROR_MT_APPLICATOR__ILLEGAL_STATE eUuid eventName entityName =
  "Failed Event (" ++ (U.toString eUuid) ++ "): You can't apply " ++ eventName ++ " to " ++ entityName

_ERROR_MT_APPLICATOR__EDIT_NON_EXISTING_THING eUuid =
  "Failed Event (" ++ (U.toString eUuid) ++ "): Try to edit non-existing thing."

_ERROR_MT_APPLICATOR__EMPTY_PATH eUuid =
  "Failed Event (" ++ (U.toString eUuid) ++ "): Applicator ends with empty path but the event wasn't applied yet."

_ERROR_MT_APPLICATOR__KM_AND_EVENT_NODE_UUID_DOES_NOT_MATCH eUuid path =
  "Failed Event (" ++
  (U.toString eUuid) ++
  "): KM Node Uuid doesn't match with Event Node Uuid (actual path: " ++ (showEventPathShort path) ++ ")"

_ERROR_MT_APPLICATOR__PATH_SHOULD_BE_EMPTY eUuid path =
  "Failed Event (" ++ (U.toString eUuid) ++ "): Path should be empty (actual path: " ++ (showEventPathShort path) ++ ")"

-- Uniqueness
_ERROR_MT_VALIDATION_APPLICATOR__KM_UNIQUENESS = "Knowledge Model is already created"

-- ------------
-- 2. Migrator
-- ------------
_ERROR_MT_MIGRATOR__TARGET_PKG_IS_NOT_HIGHER = "Target Package is not higher than the current one"

_ERROR_MT_MIGRATOR__BRANCH_HAS_TO_HAVE_MERGE_CHECKPOINT = "Branch has to have a merge checkpoint"

_ERROR_MT_MIGRATOR__BRANCH_HAS_TO_HAVE_CHECKPOINT_ABOUT_LAST_MERGED_PARENT_PKG =
  "Branch has to have a checkpoint being the last parent package that was merged into"

_ERROR_MT_MIGRATOR__NO_CONFLICTS_TO_SOLVE =
  "You can't solve conflicts because Migration state isn't in a conflict state"

_ERROR_MT_MIGRATOR__NO_EVENTS_IN_TARGET_PKG_EVENT_QUEUE = "No events in target package event queue"

_ERROR_MT_MIGRATOR__EDIT_ACTION_HAS_TO_PROVIDE_TARGET_EVENT = "Edit migration action has to provide a target event"

_ERROR_MT_MIGRATOR__ORIGINAL_EVENT_UUID_DOES_NOT_MARCH_WITH_CURRENT_TARGET_EVENT =
  "OriginalEventUuid doesn't match with the current target event"

-- Absence
_ERROR_MT_VALIDATION_MIGRATOR__SOURCE_BRANCH_ABSENCE = "Source branch does not exist"

_ERROR_MT_VALIDATION_MIGRATOR__BRANCH_PARENT_ABSENCE = "Branch has to have a parent"

_ERROR_MT_VALIDATION_MIGRATOR__TARGET_PARENT_PKG_ABSENCE = "Target parent package doesn’t exist"

-- Uniqueness
_ERROR_MT_VALIDATION_MIGRATOR__MIGRATION_UNIQUENESS = "Migration is already created"
