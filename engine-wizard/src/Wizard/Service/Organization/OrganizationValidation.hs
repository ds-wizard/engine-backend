module Wizard.Service.Organization.OrganizationValidation where

import Control.Lens ((^.))
import Control.Monad.Except (throwError)
import Data.Foldable (forM_)
import Data.Maybe (isJust)
import Text.Regex (matchRegex, mkRegex)

import LensesConfig
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Wizard.Api.Resource.Organization.OrganizationChangeDTO
import Wizard.Model.Context.AppContext

validateOrganizationDto :: OrganizationChangeDTO -> AppContextM ()
validateOrganizationDto reqDto = forM_ (isValidOrganizationId $ reqDto ^. organizationId) throwError

isValidOrganizationId :: String -> Maybe AppError
isValidOrganizationId kmId =
  if isJust $ matchRegex validationRegex kmId
    then Nothing
    else Just $ ValidationError [] [("organizationId", _ERROR_VALIDATION__INVALID_ORG_ID_FORMAT)]
  where
    validationRegex = mkRegex "^[a-zA-Z0-9][a-zA-Z0-9.]*[a-zA-Z0-9]$"
