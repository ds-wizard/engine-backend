module Registry.Localization.Messages.Public where

import Shared.Model.Localization.LocaleRecord

-- --------------------------------------
-- VALIDATION
-- --------------------------------------
-- Form
_ERROR_VALIDATION__INVALID_ORGANIZATION_ID_FORMAT =
  LocaleRecord "error.validation.invalid_organization_id_format" "OrganizationId is not in the valid format" []

-- Uniqueness
_ERROR_VALIDATION__ENTITY_UNIQUENESS entity entityIdentificator =
  LocaleRecord "error.validation.entity_uniqueness" "%s: '%s' already exists" [entity, entityIdentificator]

-- Absence
_ERROR_VALIDATION__PARENT_PKG_ABSENCE =
  LocaleRecord "error.validation.parent_pkg_absence" "Parent package doesn't exist" []

-- --------------------------------------
-- MODEL
-- --------------------------------------
-- AppContext
_ERROR_MODEL_APPCONTEXT__MISSING_ORGANIZATION =
  LocaleRecord "error.model.appcontext.missing_organization" "You have to be log in to run." []

-- --------------------------------------
-- SERVICE
-- --------------------------------------
-- Organization
_ERROR_SERVICE_ORGANIZATION__REQUIRED_HASH_IN_QUERY_PARAMS =
  LocaleRecord "error.service.organization.required_hash_in_query_params" "A hash query param has to be provided" []
