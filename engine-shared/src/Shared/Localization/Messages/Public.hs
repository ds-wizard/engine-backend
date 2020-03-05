module Shared.Localization.Messages.Public where

import Shared.Model.Localization.LocaleRecord

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
-- Absence
_ERROR_VALIDATION__FILE_ABSENCE = LocaleRecord "error.validation.file_absence" "Missing file" []

-- Format
_ERROR_VALIDATION__INVALID_KM_ID_FORMAT =
  LocaleRecord "error.validation.invalid_km_id_format" "KmId is not in the valid format" []

_ERROR_VALIDATION__INVALID_ORG_ID_FORMAT =
  LocaleRecord "error.validation.invalid_org_id_format" "OrganizationId is not in the valid format" []

-- Uniqueness
_ERROR_VALIDATION__KM_ID_UNIQUENESS kmId =
  LocaleRecord "error.validation.km_id_uniqueness" "KmId '%s' already exists" [kmId]

_ERROR_VALIDATION__PKG_ID_UNIQUENESS pkgId =
  LocaleRecord "error.validation.pkg_id_uniqueness" "Package '%s' already exists" [pkgId]

_ERROR_VALIDATION__USER_EMAIL_UNIQUENESS email =
  LocaleRecord "error.validation.user_email_uniqueness" "User (email: '%s') already exists" [email]

-- Security
_ERROR_VALIDATION__FORBIDDEN action = LocaleRecord "error.validation.forbidden" "Forbidden to perform '%s'" [action]

-- --------------------------------------
-- SERVICE
-- --------------------------------------
-- Package
_ERROR_SERVICE_PKG__HIGHER_NUMBER_IN_NEW_VERSION =
  LocaleRecord
    "error.service.pkg.highest_number_in_new_version"
    "Your new version has to be higher than the previous version"
    []

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
