module Wizard.Service.Registry.RegistryMapper where

import Control.Lens ((^.))

import LensesConfig
import Registry.Api.Resource.Organization.OrganizationCreateDTO
import Wizard.Api.Resource.Registry.RegistryCreateDTO
import Wizard.Model.Config.AppConfig

toOrganizationCreate :: AppConfig -> RegistryCreateDTO -> OrganizationCreateDTO
toOrganizationCreate appConfig reqDto =
  OrganizationCreateDTO
    { _organizationCreateDTOOrganizationId = appConfig ^. organization . organizationId
    , _organizationCreateDTOName = appConfig ^. organization . name
    , _organizationCreateDTODescription = appConfig ^. organization . description
    , _organizationCreateDTOEmail = reqDto ^. email
    }
