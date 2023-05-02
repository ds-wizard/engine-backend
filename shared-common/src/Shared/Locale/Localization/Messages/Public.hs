module Shared.Locale.Localization.Messages.Public where

import Shared.Common.Model.Localization.LocaleRecord

-- --------------------------------------
-- SERVICE
-- --------------------------------------
-- Locale
_ERROR_SERVICE_LB__MISSING_LOCALE_JSON =
  LocaleRecord
    "error.service.lb.missing_locale_json"
    "Desired definition ('locale.json') wasn't found in archive"
    []

_ERROR_SERVICE_LB__UNABLE_TO_DECODE_LOCALE_JSON errorMessage =
  LocaleRecord
    "error.service.lb.unable_to_decode_locale_json"
    "Desired definition ('locale.json') couldn't be decoded (error: '%s')"
    [errorMessage]

_ERROR_SERVICE_LB__MISSING_FILE fileName =
  LocaleRecord "error.service.lb.missing_file" "Desired file ('%s') wasn't found in zip" [fileName]
