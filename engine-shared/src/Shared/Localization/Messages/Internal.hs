module Shared.Localization.Messages.Internal where

-- --------------------------------------
-- DATABASE
-- --------------------------------------
_ERROR_DATABASE__DESERIALIZATION_FAILED = "Problem with deserialization of entity from database"

-- --------------------------------------
-- SERVICE
-- --------------------------------------
-- Config
_ERROR_SERVICE_CONFIG__VALIDATION_SECRET =
  "Invalid configuration supplied: general.secret must have 32 ASCII characters"

_ERROR_SERVICE_CONFIG__VALIDATION_SERVER_PORT =
  "Invalid configuration supplied: general.serverPort must be between 1 and 65535"

-- File
_ERROR_SERVICE_FILE__CANT_READ_JSON filename =
  "Server was unable to decode a file ('" ++ filename ++ "') to a JSON object"
