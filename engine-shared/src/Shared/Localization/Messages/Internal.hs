module Shared.Localization.Messages.Internal where

-- --------------------------------------
-- DATABASE
-- --------------------------------------
_ERROR_DATABASE__DESERIALIZATION_FAILED = "Problem with deserialization of entity from database"

-- --------------------------------------
-- SERVICE
-- --------------------------------------
-- File
_ERROR_SERVICE_FILE__CANT_READ_JSON filename =
  "Server was unable to decode a file ('" ++ filename ++ "') to a JSON object"
