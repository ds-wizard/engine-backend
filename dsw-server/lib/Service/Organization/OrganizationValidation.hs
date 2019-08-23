module Service.Organization.OrganizationValidation where

import Control.Lens ((^.))
import Data.Maybe (isJust)
import Text.Regex (matchRegex, mkRegex)

import Api.Resource.Organization.OrganizationChangeDTO
import LensesConfig
import Localization
import Model.Error.Error
import Model.Error.ErrorHelpers

validateOrganizationDto :: OrganizationChangeDTO -> Maybe AppError
validateOrganizationDto reqDto = isValidOrganizationId $ reqDto ^. organizationId

isValidOrganizationId :: String -> Maybe AppError
isValidOrganizationId kmId =
  if isJust $ matchRegex validationRegex kmId
    then Nothing
    else Just . createErrorWithFieldError $ ("organizationId", _ERROR_VALIDATION__INVALID_ORGANIZATION_ID_FORMAT)
  where
    validationRegex = mkRegex "^[a-zA-Z0-9][a-zA-Z0-9.]*[a-zA-Z0-9]$"

-- --------------------------------
-- HELPERS
-- --------------------------------
hmValidateOrganizationDto organizationDto callback =
  case validateOrganizationDto organizationDto of
    Nothing -> callback
    Just error -> return . Left $ error
