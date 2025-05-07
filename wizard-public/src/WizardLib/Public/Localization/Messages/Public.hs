module WizardLib.Public.Localization.Messages.Public where

import Shared.Common.Model.Localization.LocaleRecord

-- --------------------------------------
-- VALIDATION
-- --------------------------------------
-- Delete
_ERROR_VALIDATION__DEFAULT_LOCALE_DELETATION =
  LocaleRecord "error.validation.default_locale_deletation" "You can't delete default locale" []

_ERROR_VALIDATION__DEFAULT_WIZARD_LOCALE_DELETATION =
  LocaleRecord "error.validation.default_locale_deletation" "You can't delete default wizard locale" []

-- Uniqueness
_ERROR_VALIDATION__LCL_ID_UNIQUENESS lclId =
  LocaleRecord "error.validation.lcl_id_uniqueness" "Locale '%s' already exists" [lclId]

-- Locale
_ERROR_VALIDATION__DEACTIVATE_DEFAULT_LOCALE =
  LocaleRecord "error.validation.deactivate_default_locale" "You can't deactivate default locale" []

_ERROR_VALIDATION__LOCALE_DISABLED_DEFAULT =
  LocaleRecord "error.validation.locale_disabled_default" "You can't set disabled locale as default" []

-- --------------------------------------
-- SERVICE
-- --------------------------------------
-- OpenId
_ERROR_SERVICE_OPENID__UNABLE_TO_ENCODE_JWT_TOKEN error =
  LocaleRecord "error.service.auth.unable_to_encode_jwt_token" "Unable to encode JWT token (error: %s)" [error]

-- Token
_ERROR_SERVICE_TOKEN__INCORRECT_EMAIL_OR_PASSWORD =
  LocaleRecord "error.service.token.incorrect_email_or_password" "Incorrect email or password" []

_ERROR_SERVICE_TOKEN__INCORRECT_CODE =
  LocaleRecord "error.service.token.incorrect_code" "Incorrect code" []

_ERROR_SERVICE_TOKEN__ACCOUNT_IS_NOT_ACTIVATED =
  LocaleRecord "error.service.token.account_is_not_activated" "The account is not activated" []

_ERROR_SERVICE_TOKEN__UNABLE_TO_DECODE_AND_VERIFY_TOKEN =
  LocaleRecord "error.service.token.unable_to_decode_token" "Unable to decode and verify token" []

_ERROR_SERVICE_TOKEN__OBSOLETE_TOKEN_VERSION =
  LocaleRecord "error.service.token.obsolete_token_version" "Obsolete token version" []

_ERROR_SERVICE_TOKEN__UNABLE_TO_GET_TOKEN_VERSION =
  LocaleRecord "error.service.token.unable_to_get_token_version" "Unable to get token version from token" []

_ERROR_SERVICE_TOKEN__CODE_IS_EXPIRED = LocaleRecord "error.service.token.code_expired" "Code is expired" []

_ERROR_SERVICE_TOKEN__TOKEN_IS_EXPIRED = LocaleRecord "error.service.token.token_expired" "Token is expired" []

_ERROR_SERVICE_TOKEN__UNKNOWN_TECHNICAL_DIFFICULTIES =
  LocaleRecord "error.service.token.unknown_technical_difficulties" "Unknown technical difficulties" []

_ERROR_SERVICE_TOKEN__UNABLE_TO_GET_TOKEN_EXPIRATION =
  LocaleRecord "error.service.token.unable_to_get_token_expiration" "Unable to get token expiration from token" []

-- User
_ERROR_SERVICE_USER__MISSING_USER =
  LocaleRecord "error.service.user.missing_user" "You have to be log in to perform this request." []
