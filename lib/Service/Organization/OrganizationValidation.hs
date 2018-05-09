module Service.Organization.OrganizationValidation where

import Control.Lens ((^.))
import Data.Maybe (isJust)
import Text.Regex (matchRegex, mkRegex)

import Api.Resource.Organization.OrganizationDTO
import Common.Error
import Common.Localization
import LensesConfig

validateOrganizationDto :: OrganizationDTO -> Maybe AppError
validateOrganizationDto reqDto = isValidOrganizationId $ reqDto ^. organizationId

isValidOrganizationId :: String -> Maybe AppError
isValidOrganizationId artifactId =
  if isJust $ matchRegex validationRegex artifactId
    then Nothing
    else Just . createErrorWithFieldError $ ("groupId", _ERROR_VALIDATION__INVALID_GROUPID_FORMAT)
  where
    validationRegex = mkRegex "^[a-zA-Z0-9][a-zA-Z0-9.]*[a-zA-Z0-9]$"
