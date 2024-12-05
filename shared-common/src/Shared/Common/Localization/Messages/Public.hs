module Shared.Common.Localization.Messages.Public where

import Shared.Common.Model.Localization.LocaleRecord
import Shared.Common.Util.String (printTuples)

-- --------------------------------------
-- API
-- --------------------------------------
-- Common
_ERROR_API_COMMON__CANT_DESERIALIZE_OBJ =
  LocaleRecord "error.api.common.cant_deserialize_obj" "Problem in deserialization of JSON" []

_ERROR_API_COMMON__UNABLE_TO_GET_ORGANIZATION =
  LocaleRecord "error.api.common.unable_to_get_organization" "Unable to get organization from token header" []

_ERROR_API_COMMON__UNABLE_TO_GET_TOKEN = LocaleRecord "error.api.common.unable_to_get_token" "Unable to get token" []

_ERROR_VALIDATION__TENANT_OR_ACTIVE_PLAN_ABSENCE host = LocaleRecord "error.validation.tenant_or_active_plan_absence" "Tenant ('%s') doesn't exist or does not have any active plan" [host]

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
_ERROR_VALIDATION__FORBIDDEN_CHARACTERS word =
  LocaleRecord "error.validation.forbidden_characters" "There were forbidden characters in the '%s'" [word]

_ERROR_VALIDATION__FIELDS_ABSENCE =
  LocaleRecord "error.validation.fields_absence" "Missing fields" []

-- Security
_ERROR_VALIDATION__FORBIDDEN action = LocaleRecord "error.validation.forbidden" "Forbidden to perform '%s'" [action]

-- --------------------------------------
-- SERVICE
-- --------------------------------------
-- Common
_ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED featureName =
  LocaleRecord "error.service.common.feature_is_disabled" "Feature '%s' is disabled" [featureName]

-- Tenant
_ERROR_SERVICE_TENANT__LIMIT_EXCEEDED name maxLimit actualLimit =
  LocaleRecord
    "error.service.tenant.limit_exceeded"
    "Limit of %s exceeded (actual: %s, max: %s)"
    [name, show maxLimit, show actualLimit]

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
