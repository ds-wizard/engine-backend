module Wizard.Localization.Messages.Internal where

-- --------------------------------------
-- SERVICE
-- --------------------------------------
-- Feedback
_ERROR_SERVICE_FEEDBACK__REQUEST_FAILED serviceName endpoint =
  "Request to external API failed (service: '" ++ serviceName ++ "', endpoint: '" ++ endpoint ++ "')"

-- Mail
_MESSAGE_SERVICE_MAIL__APP_TITLE = "Data Stewardship Wizard"

-- Migration / Metamodel
_ERROR_SERVICE_MIGRATION_METAMODEL__FAILED_TO_MIGRATE_ENTITIES entityName = "Failed to migrate '" ++ entityName ++ "'"

_ERROR_SERVICE_MIGRATION_METAMODEL__FAILED_CONVERT_TO_NEW_METAMODEL entityName =
  "Failed to convert entity ('" ++ entityName ++ "') to new metamodel"

-- Project
_ERROR_SERVICE_PROJECT__INVITATION_EMAIL_NOT_SENT =
  "The project invitation email could not be sent. Please contact administrator."

-- Token
_ERROR_SERVICE_TOKEN__UNABLE_TO_GET_TOKEN = "Unable to get token"

-- User
_ERROR_SERVICE_USER__ACTIVATION_EMAIL_NOT_SENT = "The activation email could not be sent. Please contact administrator."

_ERROR_SERVICE_USER__RECOVERY_EMAIL_NOT_SENT = "The recovery email could not be sent. Please contact administrator."
