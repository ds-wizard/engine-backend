module Shared.Common.Localization.Messages.Internal where

import Shared.Common.Util.String (f')

-- --------------------------------------
-- DATABASE
-- --------------------------------------
_ERROR_DATABASE__DESERIALIZATION_FAILED = "Problem with deserialization of entity from database"

_ERROR_DATABASE__TRANSACTION_REQUIRED_DB_CONN = "Transaction requires reserved DB connection"

-- --------------------------------------
-- INTEGRATION
-- --------------------------------------
-- Common
_ERROR_INTEGRATION_COMMON__INT_SERVICE_RETURNED_ERROR message =
  "Integration Service returned an error (" ++ message ++ ")"

-- Response Mappers (Response deserialization problem)
_ERROR_INTEGRATION_COMMON__UNABLE_TO_DESERIALIZE_RESPONSE_BODY a =
  "Problem with a response deserialization (unable to deserialize a response body: " ++ a ++ ")"

_ERROR_INTEGRATION_COMMON__UNABLE_TO_GET_RESPONSE_BODY =
  "Problem with a response deserialization (unable to get a response body)"

_ERROR_INTEGRATION_COMMON__UNABLE_TO_EXTRACT_NESTED_FIELDS fieldNames =
  "Problem with a response deserialization (unable to extract nested field '" ++ show fieldNames ++ "')"

_ERROR_INTEGRATION_COMMON__UNABLE_TO_EXTRACT_STRING_FIELD fieldName =
  "Problem with a response deserialization (unable to extract string field: '" ++ fieldName ++ "')"

_ERROR_INTEGRATION_COMMON__UNABLE_TO_EXTRACT_INTEGER_FIELD fieldName =
  "Problem with a response deserialization (unable to extract integer field: '" ++ fieldName ++ "')"

_ERROR_INTEGRATION_COMMON__FIELD_IS_NOT_ARRAY = "Problem with a response deserialization (field is not an array)"

-- --------------------------------------
-- S3
-- --------------------------------------
_ERROR_S3__GENERIC_ERROR error = f' "Error in S3: %s" [error]

-- --------------------------------------
-- SERVICE
-- --------------------------------------
-- Config
_ERROR_SERVICE_CONFIG__VALIDATION_SECRET =
  "Invalid configuration supplied: general.secret must have 32 ASCII characters"

_ERROR_SERVICE_CONFIG__VALIDATION_SERVER_PORT =
  "Invalid configuration supplied: general.serverPort must be between 1 and 65535"

_ERROR_SERVICE_CONFIG__VALIDATION_RSA_PRIVATE_KEY_FORMAT = "There is no RSA private key configured"

_ERROR_SERVICE_CONFIG__VALIDATION_CFG_RSA_PRIVATE_KEY_FORMAT = "Unable to load RSA private key from the following property 'general.rsaPrivateKey'"

_ERROR_SERVICE_CONFIG__VALIDATION_ENV_RSA_PRIVATE_KEY_FORMAT = "Unable to load RSA private key from the env 'GENERAL_RSA_PRIVATE_KEY' variable"

-- File
_ERROR_SERVICE_FILE__CANT_READ_JSON filename =
  f' "Server was unable to decode a file ('%s') to a JSON object" [filename]
