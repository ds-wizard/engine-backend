module Wizard.Service.Registry.RegistryService where

import Control.Lens ((&), (.~), (^.))

import LensesConfig
import Registry.Api.Resource.Organization.OrganizationDTO
import Wizard.Api.Resource.Registry.RegistryConfirmationDTO
import Wizard.Api.Resource.Registry.RegistryCreateDTO
import Wizard.Integration.Http.Registry.Runner
import Wizard.Model.Context.AppContext
import Wizard.Service.Config.AppConfigService
import Wizard.Service.Registry.RegistryMapper

signUpToRegistry :: RegistryCreateDTO -> AppContextM OrganizationDTO
signUpToRegistry reqDto = do
  appConfig <- getAppConfig
  let orgCreateDto = toOrganizationCreate appConfig reqDto
  createOrganization orgCreateDto

confirmRegistration :: RegistryConfirmationDTO -> AppContextM OrganizationDTO
confirmRegistration reqDto = do
  org <- confirmOrganizationRegistration reqDto
  appConfig <- getAppConfig
  let updatedAppConfig = appConfig & knowledgeModelRegistry . token .~ (org ^. token)
  modifyAppConfig updatedAppConfig
  return org