module WizardLib.Public.Localization.Messages.Public where

import Shared.Common.Model.Localization.LocaleRecord

-- --------------------------------------
-- SERVICE
-- --------------------------------------
-- OpenId
_ERROR_SERVICE_OPENID__UNABLE_TO_ENCODE_JWT_TOKEN error =
  LocaleRecord "error.service.auth.unable_to_encode_jwt_token" "Unable to encode JWT token (error: %s)" [error]
