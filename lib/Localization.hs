module Localization where

import qualified Data.UUID as U

import Model.KnowledgeModel.Path

-- --------------------------------------
-- DATABASE
-- --------------------------------------
_ERROR_DATABASE__DESERIALIZATION_FAILED = "Problem with deserialization of entity from database"

_ERROR_DATABASE__ENTITY_NOT_FOUND entityName identificator =
  "Entity does not exist (" ++ entityName ++ ": " ++ identificator ++ ")"

-- --------------------------------------
-- VALIDATION
-- --------------------------------------
-- Form
_ERROR_VALIDATION__INVALID_KM_ID_FORMAT = "KmId is not in the valid format"

_ERROR_VALIDATION__INVALID_PKG_VERSION_FORMAT = "Version is not in the valid format"

_ERROR_VALIDATION__INVALID_ORGANIZATION_ID_FORMAT = "OrganizationId is not in the valid format"

_ERROR_VALIDATION__UNSUPPORTED_DMP_FORMAT format =
  "This Data Management Plan format (" ++ format ++ ") is not currently supported"

-- Uniqueness
_ERROR_VALIDATION__USER_EMAIL_UNIQUENESS email = "User with email '" ++ email ++ "' already exists"

_ERROR_VALIDATION__KM_ID_UNIQUENESS kmId = "KmId '" ++ kmId ++ "' is already taken"

_ERROR_VALIDATION__PKG_ID_UNIQUENESS pkgId = "Package '" ++ pkgId ++ "' already exists"

-- Absence
_ERROR_VALIDATION__PREVIOUS_PKG_ABSENCE = "Previous package doesn't exist"

_ERROR_VALIDATION__TEMPLATE_ABSENCE = "Template doesn't exist"

-- Security
_ERROR_VALIDATION__FORBIDDEN action = "Forbidden to perform '" ++ action ++ "'"

-- --------------------------------------
-- INTEGRATION
-- --------------------------------------
-- Common
_ERROR_INTEGRATION_COMMON__INT_SERVICE_RETURNED_ERROR statusCode =
  "Integration Service returned an error (statusCode: " ++ (show statusCode) ++ ")"

-- Response Mappers (Response deserialization problem = RDF)
_ERROR_INTEGRATION_COMMON__RDF_UNABLE_TO_DESERIALIZE_RESPONSE_BODY a =
  "Problem with a response deserialization (unable to deserialize a response body: " ++ a ++ ")"

_ERROR_INTEGRATION_COMMON__RDF_UNABLE_TO_GET_RESPONSE_BODY =
  "Problem with a response deserialization (unable to get a response body)"

_ERROR_INTEGRATION_COMMON__RDF_UNABLE_TO_EXTRACT_NESTED_FIELDS fieldNames =
  "Problem with a response deserialization (unable to extract nested field '" ++ show fieldNames ++ "')"

_ERROR_INTEGRATION_COMMON__RDF_UNABLE_TO_EXTRACT_STRING_FIELD fieldName =
  "Problem with a response deserialization (unable to extract string field: '" ++ fieldName ++ "')"

_ERROR_INTEGRATION_COMMON__RDF_FIELD_IS_NOT_ARRAY = "Problem with a response deserialization (field is not an array)"

-- --------------------------------------
-- SERVICE
-- --------------------------------------
-- Common
_ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED featureName = "'" ++ featureName ++ "' feature is disabled"

-- Document
_ERROR_SERVICE_DOCUMENT__TRANSFORMATION_FAILED err = "Couldn't transform to desired document format: " ++ err

_ERROR_SERVICE_DOCUMENT__UKNOWN_FORMAT = "Unprocessable DMP format"

-- File
_ERROR_SERVICE_FILE__CANT_READ_JSON filename =
  "Server was unable to decode a file ('" ++ filename ++ "') to a JSON object"

-- Mail
_ERROR_SERVICE_MAIL__AUTH_ERROR_MESSAGE = "Could not authenticate with SMTP server"

_ERROR_SERVICE_MAIL__EMAIL_SENT_OK recipients = "Email has been sent to: " ++ unwords recipients

_ERROR_SERVICE_MAIL__EMAIL_SENT_FAIL errorMessage = "Failed to send email: " ++ errorMessage

_ERROR_SERVICE_MAIL__FILE_LOAD_FAIL errorMessage = "Error occured while loading file: " ++ errorMessage

_ERROR_SERVICE_MAIL__MISSING_HTML mailName = "Could not load HTML template for: " ++ mailName

_ERROR_SERVICE_MAIL__MISSING_PLAIN mailName = "Could not load plain text template for: " ++ mailName

_ERROR_SERVICE_MAIL__MISSING_HTML_PLAIN mailName = "Could not load HTML nor plain text template for: " ++ mailName

_ERROR_SERVICE_MAIL__TRIED_SEND_TO_NOONE = "Tried to send email to no recipients"

_MESSAGE_SERVICE_MAIL__APP_TITLE = "Data Stewardship Wizard"

-- Migration / Metamodel
_ERROR_SERVICE_MIGRATION_METAMODEL__FAILED_TO_MIGRATE_COLLECTION collection = "Failed to migrate '" ++ collection ++ "'"

_ERROR_SERVICE_MIGRATION_METAMODEL__FAILED_CONVERT_TO_BSON entityName =
  "Failed to convert entity ('" ++ entityName ++ "') to BSON"

-- Migration / Metamodel
_ERROR_SERVICE_MIGRATION_QTN__MIGRATION_UNIQUENESS = "Migration is already created"

-- Package
_ERROR_SERVICE_PKG__IMPORT_PREVIOUS_PKG_AT_FIRST previousPkgId pkgId =
  "The previous ('" ++
  previousPkgId ++ "') of imported package ('" ++ pkgId ++ "') is missing. Please import the previous package first."

_ERROR_SERVICE_PKG__HIGHER_NUMBER_IN_NEW_VERSION = "A new version has to be higher than the previous one"

_ERROR_SERVICE_PKG__PKG_CANT_BE_DELETED_BECAUSE_IT_IS_USED_BY_SOME_OTHER_ENTITY pkgId target =
  "Package '" ++ pkgId ++ "' can't be deleted. It's used by some " ++ target ++ "."

_ERROR_SERVICE_PKG__PKG_ID_MISMATCH pkgId = "Package ID '" ++ pkgId ++ "' doesn't correspond with Package Coordinates"

-- Package Bundle
_ERROR_SERVICE_PB__MAIN_PKG_ABSENCE = "Package Bundle doesn't contain main package"

_ERROR_SERVICE_PB__PULL_NON_EXISTING_PKG pkgId = "Desired knowledge model '" ++ pkgId ++ "' wasn't found in Registry"

-- Public Questionnaire
_ERROR_SERVICE_PQ__NOT_SET_UP = "Public questionnaire is not set up"

-- Questionnaire
_ERROR_SERVICE_QTN__QTN_CANT_BE_DELETED_BECAUSE_IT_IS_USED_IN_MIGRATION =
  "Questionnaire can't be deleted because it's used in some questionnaire migration"

-- Template
_ERROR_SERVICE_TEMPLATE__LOADING_TEMPLATE_FAILED reason = "Couldn't load a template from file (" ++ reason ++ ")"

_ERROR_SERVICE_TEMPLATE__NO_TEMPLATES_IN_SYSTEM = "There are no specified templates in the system."

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

_ERROR_SERVICE_TOKEN__USER_ABSENCE userUuid = "User (" ++ userUuid ++ ") doesn't exist"

-- Template
_ERROR_SERVICE_TYPEHINT__NON_EXISTING_QUESTION = "Desired question doesn't exist"

_ERROR_SERVICE_TYPEHINT__BAD_TYPE_OF_QUESTION = "Desired question has to be Integration question"

_ERROR_SERVICE_TYPEHINT__NON_EXISTING_INTEGRATION = "Desired integrations doesn't exist"

-- User
_ERROR_SERVICE_USER__REQUIRED_ADMIN_ROLE_OR_HASH_IN_QUERY_PARAMS =
  "You have to log in as an Administrator or you have to provide a hash in the query param"

_ERROR_SERVICE_USER__REQUIRED_HASH_IN_QUERY_PARAMS = "A hash query param has to be provided"

_ERROR_SERVICE_USER__MISSING_USER = "You have to be log in to run."

_ERROR_SERVICE_USER__ACTIVATION_EMAIL_NOT_SENT = "The activation email could not be sent. Please contact administrator."

_ERROR_SERVICE_USER__RECOVERY_EMAIL_NOT_SENT = "The recovery email could not be sent. Please contact administrator."

-- --------------------------------------
-- HTTP CLIENT
-- --------------------------------------
_ERROR_HTTP_CLIENT__REQUEST_FAILED serviceName endpoint =
  "Request to external API failed (service: '" ++ serviceName ++ "', endpoint: '" ++ endpoint ++ "')"

-- --------------------------------------
-- UTIL
-- --------------------------------------
-- JSON
_ERROR_UTIL_JSON__VALUE_IS_NOT_OBJECT = "Provided value is not an object"

_ERROR_UTIL_JSON__MISSING_FIELD_IN_OBJECT fieldName = "Missing '" ++ fieldName ++ "' key in provided object"

_ERROR_UTIL_JSON__BAD_FIELD_TYPE fieldName fieldType =
  "Type of '" ++ fieldName ++ "' in provided object is not '" ++ fieldType ++ "'"

_ERROR_UTIL_JSON__CANT_DESERIALIZE_FIELD fieldName error =
  "Problem with deserialization of a field '" ++ fieldName ++ "' (error: " ++ error ++ ")"

-- --------------------------------------
-- KNOWLEDGE MODEL MIGRATION TOOL
-- --------------------------------------
-- --------------
-- 1. Applicator
-- --------------
_ERROR_KMMT_APPLICATOR__UNSPECIFIED_ERROR = "Unspecified problem in building Knowledge Model happened"

_ERROR_KMMT_APPLICATOR__CREATE_KM_AT_FIRST = "You have to create a Knowledge Model first"

_ERROR_KMMT_APPLICATOR__ILLEGAL_STATE eUuid eventName entityName =
  "Failed Event (" ++ (U.toString eUuid) ++ "): You can't apply " ++ eventName ++ " to " ++ entityName

_ERROR_KMMT_APPLICATOR__EDIT_NON_EXISTING_THING eUuid =
  "Failed Event (" ++ (U.toString eUuid) ++ "): Try to edit non-existing thing."

_ERROR_KMMT_APPLICATOR__EMPTY_PATH eUuid =
  "Failed Event (" ++ (U.toString eUuid) ++ "): Applicator ends with empty path but the event wasn't applied yet."

_ERROR_KMMT_APPLICATOR__KM_AND_EVENT_NODE_UUID_DOES_NOT_MATCH eUuid path =
  "Failed Event (" ++
  (U.toString eUuid) ++
  "): KM Node Uuid doesn't match with Event Node Uuid (actual path: " ++ (showPathShort path) ++ ")"

_ERROR_KMMT_APPLICATOR__PATH_SHOULD_BE_EMPTY eUuid path =
  "Failed Event (" ++ (U.toString eUuid) ++ "): Path should be empty (actual path: " ++ (showPathShort path) ++ ")"

-- Uniqueness
_ERROR_KMMT_VALIDATION_APPLICATOR__KM_UNIQUENESS = "Knowledge Model is already created"

-- ------------
-- 2. Migrator
-- ------------
_ERROR_KMMT_MIGRATOR__TARGET_PKG_IS_NOT_HIGHER = "Target Package is not higher than the current one"

_ERROR_KMMT_MIGRATOR__BRANCH_HAS_TO_HAVE_MERGE_CHECKPOINT = "Branch has to have a merge checkpoint"

_ERROR_KMMT_MIGRATOR__BRANCH_HAS_TO_HAVE_CHECKPOINT_ABOUT_MERGED_PREVIOUS_PKG =
  "Branch has to have a checkpoint being the last previous package that was merged into"

_ERROR_KMMT_MIGRATOR__NO_CONFLICTS_TO_SOLVE =
  "You can't solve conflicts because Migration state isn't in a conflict state"

_ERROR_KMMT_MIGRATOR__NO_EVENTS_IN_TARGET_PKG_EVENT_QUEUE = "No events in target package event queue"

_ERROR_KMMT_MIGRATOR__EDIT_ACTION_HAS_TO_PROVIDE_TARGET_EVENT = "Edit migration action has to provide a target event"

_ERROR_KMMT_MIGRATOR__ORIGINAL_EVENT_UUID_DOES_NOT_MARCH_WITH_CURRENT_TARGET_EVENT =
  "OriginalEventUuid doesn't match with the current target event"

-- Absence
_ERROR_KMMT_VALIDATION_MIGRATOR__BRANCH_PREVIOUS_PKG_ABSENCE = "Branch has to have a previous package"

-- Uniqueness
_ERROR_KMMT_VALIDATION_MIGRATOR__MIGRATION_UNIQUENESS = "Migration is already created"
