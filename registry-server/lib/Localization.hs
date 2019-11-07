module Localization where

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

-- Uniqueness
_ERROR_VALIDATION__USER_EMAIL_UNIQUENESS email = "User with email '" ++ email ++ "' already exists"

_ERROR_VALIDATION__KM_ID_UNIQUENESS kmId = "KmId '" ++ kmId ++ "' is already taken"

_ERROR_VALIDATION__PKG_ID_UNIQUENESS pkgId = "Package '" ++ pkgId ++ "' already exists"

_ERROR_VALIDATION__ENTITY_UNIQUENESS entity entityIdentificator =
  entity ++ ": '" ++ entityIdentificator ++ "' already exists"

-- Absence
_ERROR_VALIDATION__PARENT_PKG_ABSENCE = "Parent package doesn't exist"

-- Security
_ERROR_VALIDATION__FORBIDDEN action = "Forbidden to perform '" ++ action ++ "'"

-- --------------------------------------
-- API
-- --------------------------------------
-- Common
_ERROR_API_COMMON__UNABLE_TO_GET_ORGANIZATION = "Unable to get organization from token header"

_ERROR_API_COMMON__UNABLE_TO_GET_TOKEN = "Unable to get token"

-- --------------------------------------
-- MODEL
-- --------------------------------------
-- AppContext
_ERROR_MODEL_APPCONTEXT__MISSING_ORGANIZATION = "You have to be log in to run."

-- --------------------------------------
-- SERVICE
-- --------------------------------------
-- Common
_ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED featureName = "'" ++ featureName ++ "' feature is disabled"

-- File
_ERROR_SERVICE_FILE__CANT_READ_JSON filename =
  "Server was unable to decode a file ('" ++ filename ++ "') to a JSON object"

-- Knowledge Model Bundle
_ERROR_SERVICE_KMB__MAIN_PKG_ABSENCE = "Knowledge Model Bundle doesn't contain main package"

-- Mail
_ERROR_SERVICE_MAIL__AUTH_ERROR_MESSAGE = "Could not authenticate with SMTP server"

_ERROR_SERVICE_MAIL__EMAIL_SENT_OK recipients = "Email has been sent to: " ++ unwords recipients

_ERROR_SERVICE_MAIL__EMAIL_SENT_FAIL errorMessage = "Failed to send email: " ++ errorMessage

_ERROR_SERVICE_MAIL__FILE_LOAD_FAIL errorMessage = "Error occured while loading file: " ++ errorMessage

_ERROR_SERVICE_MAIL__MISSING_HTML mailName = "Could not load HTML template for: " ++ mailName

_ERROR_SERVICE_MAIL__MISSING_PLAIN mailName = "Could not load plain text template for: " ++ mailName

_ERROR_SERVICE_MAIL__MISSING_HTML_PLAIN mailName = "Could not load HTML nor plain text template for: " ++ mailName

_ERROR_SERVICE_MAIL__TRIED_SEND_TO_NOONE = "Tried to send email to no recipients"

-- Organization
_ERROR_SERVICE_ORGANIZATION__ACTIVATION_EMAIL_NOT_SENT =
  "The activation email could not be sent. Please contact administrator."

_ERROR_SERVICE_ORGANIZATION__RECOVERY_EMAIL_NOT_SENT =
  "The recovery email could not be sent. Please contact administrator."

_ERROR_SERVICE_ORGANIZATION__REQUIRED_HASH_IN_QUERY_PARAMS = "A hash query param has to be provided"

-- Package
_ERROR_SERVICE_PKG__IMPORT_PARENT_PKG_AT_FIRST parentPkgId pkgId =
  "The parent ('" ++
  parentPkgId ++ "') of imported package ('" ++ pkgId ++ "') is missing. Please import the parent first."

_ERROR_SERVICE_PKG__HIGHER_NUMBER_IN_NEW_VERSION = "A new version has to be higher than the previous one"

_ERROR_SERVICE_PKG__PKG_CANT_BE_DELETED_BECAUSE_IT_IS_USED_BY_SOME_OTHER_ENTITY pkgId target =
  "Package '" ++ pkgId ++ "' can't be deleted. It's used by some " ++ target ++ "."

_ERROR_SERVICE_PKG__PKG_ID_MISMATCH pkgId = "Package ID '" ++ pkgId ++ "' doesn't correspond with Package Coordinates"

_ERROR_SERVICE_PKG__OWNERSHIP_MISMATCH pkgId = "You can not push package which you do not own ('" ++ pkgId ++ "')"

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
