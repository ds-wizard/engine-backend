module Registry.Localization.Messages.Public where

import Shared.Common.Model.Localization.LocaleRecord

-- --------------------------------------
-- VALIDATION
-- --------------------------------------
-- Absence
_ERROR_VALIDATION__ORGANIZATION_EMAIL_ABSENCE email =
  LocaleRecord
    "error.validation.organization_email_absence"
    "This email (`%s`) is not connected to any organization"
    [email]

-- Uniqueness
_ERROR_VALIDATION__ORGANIZATION_ID_UNIQUENESS orgId =
  LocaleRecord "error.validation.organization_id_uniqueness" "OrganizationId ('%s') already exists" [orgId]

_ERROR_VALIDATION__ORGANIZATION_EMAIL_UNIQUENESS email =
  LocaleRecord "error.validation.organization_email_uniqueness" "Email ('%s') already exists" [email]

-- --------------------------------------
-- MODEL
-- --------------------------------------
-- AppContext
_ERROR_MODEL_APPCONTEXT__MISSING_ORGANIZATION =
  LocaleRecord "error.model.appcontext.missing_organization" "You have to be log in to run." []
