module Service.Organization.OrganizationService where

import Control.Lens ((^.))
import Control.Monad.Reader
import Crypto.PasswordStore
import Data.ByteString.Char8 as BS
import Data.Maybe
import Data.UUID as U
import Text.Regex

import Api.Resource.Organization.OrganizationDTO
import Common.Context
import Common.Error
import Common.Localization
import Common.Types
import Common.Uuid
import Database.DAO.Organization.OrganizationDAO
import LensesConfig
import Model.Organization.Organization
import Service.Organization.OrganizationMapper

getOrganization :: Context -> IO (Either AppError OrganizationDTO)
getOrganization context = do
  eitherOrganization <- findOrganization context
  case eitherOrganization of
    Right organization -> return . Right . toDTO $ organization
    Left error -> return . Left $ error

modifyOrganization :: Context -> OrganizationDTO -> IO (Either AppError OrganizationDTO)
modifyOrganization context organizationDto =
  case isValidOrganizationId $ organizationDto ^. organizationId of
    Nothing -> do
      let organization = fromDTO organizationDto
      updateOrganization context organization
      return . Right $ organizationDto
    Just error -> return . Left $ error

isValidOrganizationId :: String -> Maybe AppError
isValidOrganizationId artifactId =
  if isJust $ matchRegex validationRegex artifactId
    then Nothing
    else Just . createErrorWithFieldError $ ("groupId", _ERROR_VALIDATION__INVALID_GROUPID_FORMAT)
  where
    validationRegex = mkRegex "^[a-zA-Z0-9][a-zA-Z0-9.]*[a-zA-Z0-9]$"
