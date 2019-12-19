module Registry.Service.Organization.OrganizationValidation where

import Control.Lens ((^.))
import Data.Maybe (isJust)
import Text.Regex (matchRegex, mkRegex)

import Registry.Api.Resource.Organization.OrganizationCreateDTO
import Registry.Database.DAO.Organization.OrganizationDAO
import Registry.LensesConfig
import Registry.Localization.Messages.Public
import Registry.Model.Context.AppContext
import Shared.Model.Error.Error
import Shared.Util.Helper (createHmeHelper, createHmmHelper)

validateOrganizationCreateDto :: OrganizationCreateDTO -> AppContextM (Maybe AppError)
validateOrganizationCreateDto reqDto =
  hmValidateOrganizationIdUniqueness (reqDto ^. organizationId) $
  hmValidateOrganizationEmailUniqueness (reqDto ^. email) $ return $ validateOrganizationId (reqDto ^. organizationId)

validateOrganizationId :: String -> Maybe AppError
validateOrganizationId orgId =
  if isJust $ matchRegex validationRegex orgId
    then Nothing
    else Just $ ValidationError [] [("organizationId", _ERROR_VALIDATION__INVALID_ORGANIZATION_ID_FORMAT)]
  where
    validationRegex = mkRegex "^[a-zA-Z0-9][a-zA-Z0-9.]*[a-zA-Z0-9]$"

validateOrganizationIdUniqueness :: String -> AppContextM (Maybe AppError)
validateOrganizationIdUniqueness orgId = do
  eOrg <- findOrganizationByOrgId orgId
  case eOrg of
    Left (NotExistsError _) -> return Nothing
    Right _ ->
      return . Just $ ValidationError [] [("organizationId", _ERROR_VALIDATION__ENTITY_UNIQUENESS "Organization" orgId)]
    Left error -> return . Just $ error

validateOrganizationEmailUniqueness :: String -> AppContextM (Maybe AppError)
validateOrganizationEmailUniqueness email = do
  eOrg <- findOrganizationByEmail email
  case eOrg of
    Left (NotExistsError _) -> return Nothing
    Right _ -> return . Just $ ValidationError [] [("email", _ERROR_VALIDATION__ENTITY_UNIQUENESS "Email" email)]
    Left error -> return . Just $ error

validateOrganizationChangedEmailUniqueness :: String -> String -> AppContextM (Maybe AppError)
validateOrganizationChangedEmailUniqueness newEmail oldEmail =
  if newEmail /= oldEmail
    then validateOrganizationEmailUniqueness newEmail
    else return Nothing

-- --------------------------------
-- HELPERS
-- --------------------------------
heValidateOrganizationCreateDto reqDto callback = createHmeHelper (validateOrganizationCreateDto reqDto) callback

-- --------------------------------
hmValidateOrganizationIdUniqueness orgId callback = createHmmHelper (validateOrganizationIdUniqueness orgId) callback

-- --------------------------------
hmValidateOrganizationEmailUniqueness email callback =
  createHmmHelper (validateOrganizationEmailUniqueness email) callback

-- --------------------------------
heValidateOrganizationChangedEmailUniqueness newEmail oldEmail callback =
  createHmeHelper (validateOrganizationChangedEmailUniqueness newEmail oldEmail) callback
