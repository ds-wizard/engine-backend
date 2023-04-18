module WizardLib.Common.Localization.Messages.Public where

import Shared.Common.Model.Localization.LocaleRecord

-- --------------------------------------
-- API
-- --------------------------------------
-- Common
_ERROR_API_COMMON__UNABLE_TO_GET_ORGANIZATION =
  LocaleRecord "error.api.common.unable_to_get_organization" "Unable to get organization from token header" []

-- --------------------------------------
-- VALIDATION
-- --------------------------------------
-- General
_ERROR_VALIDATION__COORDINATE_MISMATCH coordinate =
  LocaleRecord
    "error.validation.coordinate_mismatch"
    "Coordinate '%s' doesn't correspond with 'orgId', 'kmId/documentTemplateId' or 'version'"
    [coordinate]

-- Format
_ERROR_VALIDATION__INVALID_COORDINATE_FORMAT =
  LocaleRecord "error.validation.invalid_coordinate_format" "Coordinate is not in the valid format" []

_ERROR_VALIDATION__INVALID_COORDINATE_VERSION_FORMAT =
  LocaleRecord "error.validation.invalid_coordinate_version_format" "Version is not in the valid format" []

_ERROR_VALIDATION__INVALID_ORG_ID_FORMAT =
  LocaleRecord "error.validation.invalid_org_id_format" "OrganizationId is not in the valid format" []
