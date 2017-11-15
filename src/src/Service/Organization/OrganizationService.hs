module Service.Organization.OrganizationService where

import Control.Lens ((^.))
import Control.Monad.Reader
import Crypto.PasswordStore
import Data.ByteString.Char8 as BS
import Data.Maybe
import Data.UUID as U
import Text.Regex

import Api.Resources.Organization.OrganizationDTO
import Common.Error
import Common.Types
import Common.Uuid
import Context
import Database.DAO.Organization.OrganizationDAO
import Model.Organization.Organization
import Service.Organization.OrganizationMapper

getOrganization :: Context -> IO (Either AppError OrganizationDTO)
getOrganization context = do
  eitherOrganization <- findOrganization context
  case eitherOrganization of
    Right organization -> return . Right . toDTO $ organization
    Left error -> return . Left $ error

modifyOrganization :: Context
                   -> OrganizationDTO
                   -> IO (Either AppError OrganizationDTO)
modifyOrganization context organizationDto = do
  let groupId = organizationDto ^. orgdtoGroupId
  case isValidGroupId groupId of
    Nothing -> do
      let organization = fromDTO organizationDto
      updateOrganization context organization
      return . Right $ organizationDto
    Just error -> return . Left $ error

getOrganizationGroupId :: Context -> IO (Either AppError String)
getOrganizationGroupId context = do
  eitherOrganization <- findOrganization context
  case eitherOrganization of
    Right organization -> return . Right $ organization ^. orgGroupId
    Left error -> return . Left $ error

isValidGroupId :: String -> Maybe AppError
isValidGroupId artifactId =
  if isJust $ matchRegex validationRegex artifactId
  then Nothing
  else Just . createErrorWithFieldError $ ("groupId", "GroupId is not in valid format")
  where
    validationRegex = mkRegex "^[a-zA-Z0-9][a-zA-Z0-9.]*[a-zA-Z0-9]$"
