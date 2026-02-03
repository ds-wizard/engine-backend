module Wizard.Service.Registry.Registration.RegistryRegistrationService where

import Control.Monad.Reader (liftIO)
import Data.Time

import RegistryLib.Api.Resource.Organization.OrganizationDTO
import Wizard.Api.Resource.Registry.RegistryConfirmationDTO
import Wizard.Api.Resource.Registry.RegistryCreateDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.Tenant.Config.TenantConfigOrganizationDAO
import Wizard.Integration.Http.Registry.Runner
import Wizard.Model.Context.AppContext
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Service.Registry.RegistryMapper
import Wizard.Service.Tenant.Config.ConfigService

signUpToRegistry :: RegistryCreateDTO -> AppContextM OrganizationDTO
signUpToRegistry reqDto =
  runInTransaction $ do
    tcOrganization <- findTenantConfigOrganization
    let orgCreateDto = toOrganizationCreate tcOrganization reqDto
    createOrganization orgCreateDto

confirmRegistration :: RegistryConfirmationDTO -> AppContextM OrganizationDTO
confirmRegistration reqDto =
  runInTransaction $ do
    org <- confirmOrganizationRegistration reqDto
    tcRegistry <- getCurrentTenantConfigRegistry
    now <- liftIO getCurrentTime
    let updatedRegistry = tcRegistry {enabled = True, token = org.token, updatedAt = now}
    modifyTenantConfigRegistry updatedRegistry
    return org
