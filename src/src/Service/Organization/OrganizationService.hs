module Service.Organization.OrganizationService where

import Control.Lens ((^.))
import Control.Monad.Reader
import Crypto.PasswordStore
import Data.ByteString.Char8 as BS
import Data.UUID as U

import Api.Resources.Organization.OrganizationDTO
import Common.Types
import Common.Uuid
import Context
import Database.DAO.Organization.OrganizationDAO
import Model.Organization.Organization
import Service.Organization.OrganizationMapper

getOrganization :: Context -> IO (Maybe OrganizationDTO)
getOrganization context = do
  maybeOrganization <- findOrganization context
  case maybeOrganization of
    Just organization -> return . Just . toDTO $ organization
    _ -> return Nothing

modifyOrganization :: Context -> OrganizationDTO -> IO OrganizationDTO
modifyOrganization context organizationDto = do
  let organization = fromDTO organizationDto
  updateOrganization context organization
  return organizationDto
