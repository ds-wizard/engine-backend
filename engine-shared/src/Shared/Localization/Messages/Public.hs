module Shared.Localization.Messages.Public where

import Shared.Model.Localization.LocaleRecord
import Shared.Util.String (printTuples)

-- --------------------------------------
-- API
-- --------------------------------------
-- Common
_ERROR_API_COMMON__CANT_DESERIALIZE_OBJ =
  LocaleRecord "error.api.common.cant_deserialize_obj" "Problem in deserialization of JSON" []

_ERROR_API_COMMON__UNABLE_TO_GET_ORGANIZATION =
  LocaleRecord "error.api.common.unable_to_get_organization" "Unable to get organization from token header" []

_ERROR_API_COMMON__UNABLE_TO_GET_TOKEN = LocaleRecord "error.api.common.unable_to_get_token" "Unable to get token" []

-- Websocket
_ERROR_API_WEBSOCKET__DESERIALIZATION_FAILED =
  LocaleRecord "error.api.websocket.cant_deserialize_obj" "Deserialization failed" []

_ERROR_API__WEBSOCKET_RECORD_NOT_FOUND connectionUUid =
  LocaleRecord
    "error.api.websocket.record_not_found"
    "Websocket record (connectionUuid: '%s') does not exist"
    [connectionUUid]

-- --------------------------------------
-- DATABASE
-- --------------------------------------
_ERROR_DATABASE__ENTITY_NOT_FOUND entityName params =
  LocaleRecord
    "error.database.entity_not_found"
    "Object of type '%s' does not exist (%s)"
    [entityName, printTuples params]

-- --------------------------------------
-- VALIDATION
-- --------------------------------------
-- General
_ERROR_VALIDATION__COORDINATE_MISMATCH coordinate =
  LocaleRecord
    "error.validation.coordinate_mismatch"
    "Coordinate '%s' doesn't correspond with 'orgId', 'kmId/documentTemplateId' or 'version'"
    [coordinate]

_ERROR_VALIDATION__FORBIDDEN_CHARACTERS word =
  LocaleRecord "error.validation.forbidden_characters" "There were forbidden characters in the '%s'" [word]

_ERROR_VALIDATION__FIELDS_ABSENCE =
  LocaleRecord "error.validation.fields_absence" "Missing fields" []

-- Absence
_ERROR_VALIDATION__MAIN_PKG_OF_PB_ABSENCE =
  LocaleRecord "error.validation.main_pkg_of_pb_absence" "Package Bundle doesn't contain main package" []

-- Format
_ERROR_VALIDATION__INVALID_COORDINATE_FORMAT =
  LocaleRecord "error.validation.invalid_coordinate_format" "Coordinate is not in the valid format" []

_ERROR_VALIDATION__INVALID_COORDINATE_VERSION_FORMAT =
  LocaleRecord "error.validation.invalid_coordinate_version_format" "Version is not in the valid format" []

_ERROR_VALIDATION__INVALID_KM_ID_FORMAT =
  LocaleRecord "error.validation.invalid_km_id_format" "KmId is not in the valid format" []

_ERROR_VALIDATION__INVALID_ORG_ID_FORMAT =
  LocaleRecord "error.validation.invalid_org_id_format" "OrganizationId is not in the valid format" []

-- Uniqueness
_ERROR_VALIDATION__DOC_TML_ID_UNIQUENESS tmlId =
  LocaleRecord "error.validation.tml_id_uniqueness" "DocumentTemplate '%s' already exists" [tmlId]

_ERROR_VALIDATION__PKG_ID_UNIQUENESS pkgId =
  LocaleRecord "error.validation.pkg_id_uniqueness" "Package '%s' already exists" [pkgId]

_ERROR_VALIDATION__USER_EMAIL_UNIQUENESS email =
  LocaleRecord "error.validation.user_email_uniqueness" "User (email: '%s') already exists" [email]

-- Security
_ERROR_VALIDATION__FORBIDDEN action = LocaleRecord "error.validation.forbidden" "Forbidden to perform '%s'" [action]

-- --------------------------------------
-- SERVICE
-- --------------------------------------
-- App
_ERROR_SERVICE_APP__LIMIT_EXCEEDED name maxLimit actualLimit =
  LocaleRecord
    "error.service.app.limit_exceeded"
    "Limit of %s exceeded (actual: %s, max: %s)"
    [name, show maxLimit, show actualLimit]

-- DocumentTemplate
_ERROR_SERVICE_TB__MISSING_TEMPLATE_JSON =
  LocaleRecord
    "error.service.tb.missing_template_json"
    "Desired definition ('template.json') wasn't found in archive"
    []

-- Locale
_ERROR_SERVICE_LB__MISSING_LOCALE_JSON =
  LocaleRecord
    "error.service.lb.missing_locale_json"
    "Desired definition ('locale.json') wasn't found in archive"
    []

_ERROR_SERVICE_LB__UNABLE_TO_DECODE_LOCALE_JSON errorMessage =
  LocaleRecord
    "error.service.lb.unable_to_decode_locale_json"
    "Desired definition ('locale.json') couldn't be decoded (error: '%s')"
    [errorMessage]

_ERROR_SERVICE_LB__MISSING_FILE fileName =
  LocaleRecord "error.service.lb.missing_file" "Desired file ('%s') wasn't found in zip" [fileName]

-- Package
_ERROR_SERVICE_PKG__HIGHER_NUMBER_IN_NEW_VERSION =
  LocaleRecord
    "error.service.pkg.highest_number_in_new_version"
    "Your new version has to be higher than the previous version"
    []

_ERROR_SERVICE_TB__UNABLE_TO_DECODE_TEMPLATE_JSON errorMessage =
  LocaleRecord
    "error.service.tb.unable_to_decode_template_json"
    "Desired definition ('template.json') couldn't be decoded (error: '%s')"
    [errorMessage]

_ERROR_SERVICE_TB__MISSING_ASSET fileName =
  LocaleRecord "error.service.tb.missing_asset" "Desired asset ('%s') wasn't found in zip" [fileName]

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
