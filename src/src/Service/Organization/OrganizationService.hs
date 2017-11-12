module Service.Organization.OrganizationService where

import Control.Lens ((^.))
import Control.Monad.Reader
import Crypto.PasswordStore
import Data.ByteString.Char8 as BS
import Data.UUID as U

import Api.Resources.Organization.OrganizationDTO
import Common.Types
import Common.Uuid
import Common.Error
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

modifyOrganization :: Context -> OrganizationDTO -> IO OrganizationDTO
modifyOrganization context organizationDto = do
  let organization = fromDTO organizationDto
  updateOrganization context organization
  return organizationDto
