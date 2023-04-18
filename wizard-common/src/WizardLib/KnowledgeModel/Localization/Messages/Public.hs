module WizardLib.KnowledgeModel.Localization.Messages.Public where

import Shared.Common.Model.Localization.LocaleRecord

-- --------------------------------------
-- VALIDATION
-- --------------------------------------
-- Absence
_ERROR_VALIDATION__MAIN_PKG_OF_PB_ABSENCE =
  LocaleRecord "error.validation.main_pkg_of_pb_absence" "Package Bundle doesn't contain main package" []

-- Format
_ERROR_VALIDATION__INVALID_KM_ID_FORMAT =
  LocaleRecord "error.validation.invalid_km_id_format" "KmId is not in the valid format" []

-- Uniqueness
_ERROR_VALIDATION__PKG_ID_UNIQUENESS pkgId =
  LocaleRecord "error.validation.pkg_id_uniqueness" "Package '%s' already exists" [pkgId]

-- --------------------------------------
-- SERVICE
-- --------------------------------------
-- Package
_ERROR_SERVICE_PKG__HIGHER_NUMBER_IN_NEW_VERSION =
  LocaleRecord
    "error.service.pkg.highest_number_in_new_version"
    "Your new version has to be higher than the previous version"
    []
