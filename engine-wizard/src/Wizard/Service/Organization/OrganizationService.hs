module Wizard.Service.Organization.OrganizationService where

import Control.Lens ((^.))
import Control.Monad.Reader (liftIO)
import Data.Time

import LensesConfig
import Wizard.Api.Resource.Organization.OrganizationChangeDTO
import Wizard.Api.Resource.Organization.OrganizationDTO
import Wizard.Database.DAO.Organization.OrganizationDAO
import Wizard.Model.Context.AppContext
import Wizard.Service.Organization.OrganizationMapper
import Wizard.Service.Organization.OrganizationValidation

getOrganization :: AppContextM OrganizationDTO
getOrganization = do
  organization <- findOrganization
  return . toDTO $ organization

modifyOrganization :: OrganizationChangeDTO -> AppContextM OrganizationDTO
modifyOrganization reqDto = do
  validateOrganizationDto reqDto
  organizationFromDB <- getOrganization
  now <- liftIO getCurrentTime
  let organization = fromDTO reqDto (organizationFromDB ^. createdAt) now
  updateOrganization organization
  return . toDTO $ organization
