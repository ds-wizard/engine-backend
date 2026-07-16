module Shared.KnowledgeModel.Localization.Messages.Public where

import Shared.Common.Model.Localization.LocaleRecord

-- --------------------------------------
-- VALIDATION
-- --------------------------------------
-- Absence
_ERROR_VALIDATION__MAIN_PKG_OF_PB_ABSENCE =
  LocaleRecord "error.validation.main_pkg_of_pb_absence" "Knowledge Model Bundle doesn't contain main package" []

-- Uniqueness
_ERROR_VALIDATION__PKG_ID_UNIQUENESS pkgId =
  LocaleRecord "error.validation.pkg_id_uniqueness" "Package '%s' already exists" [pkgId]

-- Knowledge Model Locale
_ERROR_VALIDATION__KM_LOCALE_INVALID_PO reason =
  LocaleRecord "error.validation.km_locale_invalid_po" "Unable to parse the PO file: %s" [reason]

_ERROR_VALIDATION__KM_LOCALE_MISSING_LANGUAGE =
  LocaleRecord "error.validation.km_locale_missing_language" "The PO file has no 'Language' header field" []

_ERROR_VALIDATION__KM_LOCALE_INVALID_JSON reason =
  LocaleRecord "error.validation.km_locale_invalid_json" "Unable to parse the JSON translation file: %s" [reason]

_ERROR_VALIDATION__KM_LOCALE_CODE_UNIQUENESS code =
  LocaleRecord "error.validation.km_locale_code_uniqueness" "Translation for language '%s' already exists" [code]

_ERROR_VALIDATION__KM_LOCALE_NOT_REUSABLE =
  LocaleRecord "error.validation.km_locale_not_reusable" "Selected locales cannot be reused from the previous package version" []

-- --------------------------------------
-- SERVICE
-- --------------------------------------
-- Document Template
_ERROR_SERVICE_DOC_TML__NON_EDITABLE_DOC_TML =
  LocaleRecord
    "error.service.doc_tml.non_editable_doc_tml"
    "Unable to export / edit non editable document template"
    []

-- Package
_ERROR_SERVICE_PKG__HIGHER_NUMBER_IN_NEW_VERSION =
  LocaleRecord
    "error.service.pkg.highest_number_in_new_version"
    "Your new version has to be higher than the previous version"
    []

_ERROR_SERVICE_PKG__NON_EDITABLE_PKG =
  LocaleRecord
    "error.service.pkg.non_editable_pkg"
    "Unable to export / edit non editable package"
    []
