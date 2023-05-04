module Shared.Common.Localization.Messages.Internal where

import Shared.Common.Util.String (f')

-- --------------------------------------
-- DATABASE
-- --------------------------------------
_ERROR_DATABASE__DESERIALIZATION_FAILED = "Problem with deserialization of entity from database"

_ERROR_DATABASE__TRANSACTION_REQUIRED_DB_CONN = "Transaction requires reserved DB connection"

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
