module WizardLib.DocumentTemplate.Localization.Messages.Public where

import Shared.Common.Model.Localization.LocaleRecord

-- --------------------------------------
-- VALIDATION
-- --------------------------------------
-- Uniqueness
_ERROR_VALIDATION__DOC_TML_ID_UNIQUENESS tmlId =
  LocaleRecord "error.validation.tml_id_uniqueness" "DocumentTemplate '%s' already exists" [tmlId]

-- --------------------------------------
-- SERVICE
-- --------------------------------------
-- DocumentTemplate
_ERROR_SERVICE_TB__MISSING_TEMPLATE_JSON =
  LocaleRecord
    "error.service.tb.missing_template_json"
    "Desired definition ('template.json') wasn't found in archive"
    []

_ERROR_SERVICE_TB__MISSING_ASSET fileName =
  LocaleRecord "error.service.tb.missing_asset" "Desired asset ('%s') wasn't found in zip" [fileName]

_ERROR_SERVICE_TB__UNABLE_TO_DECODE_TEMPLATE_JSON errorMessage =
  LocaleRecord
    "error.service.tb.unable_to_decode_template_json"
    "Desired definition ('template.json') couldn't be decoded (error: '%s')"
    [errorMessage]
