module Registry.Localization.Messages.Internal where

-- --------------------------------------
-- API
-- --------------------------------------
-- Common
_ERROR_API_COMMON__UNABLE_TO_GET_ORGANIZATION = "Unable to get organization from token header"

_ERROR_API_COMMON__UNABLE_TO_GET_TOKEN = "Unable to get token"

-- --------------------------------------
-- SERVICE
-- --------------------------------------
-- Common
_ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED featureName = "'" ++ featureName ++ "' feature is disabled"

-- Knowledge Model Bundle
_ERROR_SERVICE_KMB__MAIN_PKG_ABSENCE = "Knowledge Model Bundle doesn't contain main package"

-- Organization
_ERROR_SERVICE_ORGANIZATION__ACTIVATION_EMAIL_NOT_SENT =
  "The activation email could not be sent. Please contact administrator."

_ERROR_SERVICE_ORGANIZATION__RECOVERY_EMAIL_NOT_SENT =
  "The recovery email could not be sent. Please contact administrator."

-- Package
_ERROR_SERVICE_PKG__IMPORT_PARENT_PKG_AT_FIRST parentPkgId pkgId =
  "The parent ('" ++
  parentPkgId ++ "') of imported package ('" ++ pkgId ++ "') is missing. Please import the parent first."

_ERROR_SERVICE_PKG__PKG_CANT_BE_DELETED_BECAUSE_IT_IS_USED_BY_SOME_OTHER_ENTITY pkgId target =
  "Package '" ++ pkgId ++ "' can't be deleted. It's used by some " ++ target ++ "."

_ERROR_SERVICE_PKG__PKG_ID_MISMATCH pkgId = "Package ID '" ++ pkgId ++ "' doesn't correspond with Package Coordinates"

_ERROR_SERVICE_PKG__OWNERSHIP_MISMATCH pkgId = "You can not push package which you do not own ('" ++ pkgId ++ "')"
