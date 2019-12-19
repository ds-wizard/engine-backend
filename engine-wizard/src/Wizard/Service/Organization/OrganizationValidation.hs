module Wizard.Service.Organization.OrganizationValidation where

import Control.Lens ((^.))
import Data.Maybe (isJust)
import Text.Regex (matchRegex, mkRegex)

import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Wizard.Api.Resource.Organization.OrganizationChangeDTO
import Wizard.LensesConfig

validateOrganizationDto :: OrganizationChangeDTO -> Maybe AppError
validateOrganizationDto reqDto = isValidOrganizationId $ reqDto ^. organizationId

isValidOrganizationId :: String -> Maybe AppError
isValidOrganizationId kmId =
  if isJust $ matchRegex validationRegex kmId
    then Nothing
    else Just $ ValidationError [] [("organizationId", _ERROR_VALIDATION__INVALID_ORG_ID_FORMAT)]
  where
    validationRegex = mkRegex "^[a-zA-Z0-9][a-zA-Z0-9.]*[a-zA-Z0-9]$"

-- --------------------------------
-- HELPERS
-- --------------------------------
hmValidateOrganizationDto organizationDto callback =
  case validateOrganizationDto organizationDto of
    Nothing -> callback
    Just error -> return . Left $ error
