module Shared.OpenId.Localization.Messages.Public where

import Shared.Common.Model.Localization.LocaleRecord

-- --------------------------------------
-- SERVICE
-- --------------------------------------
-- Open ID
_ERROR_VALIDATION__OPENID_WRONG_RESPONSE error =
  LocaleRecord "error.validation.openid_wrong_response" "There was a wrong response from OpenID (error: '%s')" [error]

_ERROR_VALIDATION__OPENID_CODE_ABSENCE =
  LocaleRecord "error.validation.openid_code_absence" "Auth Code is not provided" []

_ERROR_VALIDATION__OPENID_PROFILE_INFO_ABSENCE =
  LocaleRecord "error.validation.openid_profile_info_absence" "Profile Information from OpenID service is missing" []
