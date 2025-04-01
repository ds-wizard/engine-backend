module WizardLib.KnowledgeModel.Localization.Messages.Public where

import Shared.Common.Model.Localization.LocaleRecord

-- --------------------------------------
-- VALIDATION
-- --------------------------------------
-- Absence
_ERROR_VALIDATION__MAIN_PKG_OF_PB_ABSENCE =
  LocaleRecord "error.validation.main_pkg_of_pb_absence" "Package Bundle doesn't contain main package" []

-- Uniqueness
_ERROR_VALIDATION__PKG_ID_UNIQUENESS pkgId =
  LocaleRecord "error.validation.pkg_id_uniqueness" "Package '%s' already exists" [pkgId]

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
