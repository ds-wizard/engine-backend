module Wizard.Service.Registry.RegistryService where

import Control.Lens ((&), (.~), (^.))

import LensesConfig
import Registry.Api.Resource.Organization.OrganizationDTO
import Wizard.Api.Resource.Registry.RegistryConfirmationDTO
import Wizard.Api.Resource.Registry.RegistryCreateDTO
import Wizard.Database.DAO.Common
import Wizard.Integration.Http.Registry.Runner
import Wizard.Model.Config.AppConfig
import Wizard.Model.Context.AppContext
import Wizard.Service.Config.AppConfigService
import Wizard.Service.Registry.RegistryMapper

signUpToRegistry :: RegistryCreateDTO -> AppContextM OrganizationDTO
signUpToRegistry reqDto =
  runInTransaction $ do
    appConfig <- getAppConfig
    let orgCreateDto = toOrganizationCreate appConfig reqDto
    createOrganization orgCreateDto

confirmRegistration :: RegistryConfirmationDTO -> AppContextM OrganizationDTO
confirmRegistration reqDto =
  runInTransaction $ do
    org <- confirmOrganizationRegistration reqDto
    appConfig <- getAppConfig
    let updatedRegistry = AppConfigRegistry {_appConfigRegistryEnabled = True, _appConfigRegistryToken = org ^. token}
    let updatedAppConfig = appConfig & registry .~ updatedRegistry
    modifyAppConfig updatedAppConfig
    return org
