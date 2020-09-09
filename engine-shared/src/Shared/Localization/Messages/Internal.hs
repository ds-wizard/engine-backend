module Shared.Localization.Messages.Internal where

-- --------------------------------------
-- API
-- --------------------------------------
_ERROR_API_COMMON__UNABLE_TO_GET_TOKEN = "Unable to get token"

-- Websocket
_ERROR_API_WEBSOCKET__DESERIALIZATION_FAILED = "Deserialization failed"

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

-- Mail
_ERROR_SERVICE_MAIL__AUTH_ERROR_MESSAGE = "Could not authenticate with SMTP server"

_ERROR_SERVICE_MAIL__EMAIL_SENT_OK recipients = "Email has been sent to: " ++ unwords recipients

_ERROR_SERVICE_MAIL__EMAIL_SENT_FAIL errorMessage = "Failed to send email: " ++ errorMessage

_ERROR_SERVICE_MAIL__FILE_LOAD_FAIL errorMessage = "Error occured while loading file: " ++ errorMessage

_ERROR_SERVICE_MAIL__MISSING_HTML mailName = "Could not load HTML template for: " ++ mailName

_ERROR_SERVICE_MAIL__MISSING_PLAIN mailName = "Could not load plain text template for: " ++ mailName

_ERROR_SERVICE_MAIL__MISSING_HTML_PLAIN mailName = "Could not load HTML nor plain text template for: " ++ mailName

_ERROR_SERVICE_MAIL__TRIED_SEND_TO_NOONE = "Tried to send email to no recipients"
