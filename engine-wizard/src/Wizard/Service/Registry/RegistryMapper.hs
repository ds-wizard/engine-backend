module Wizard.Service.Registry.RegistryMapper where

import Control.Lens ((^.))
import Data.Time

import LensesConfig
import Registry.Api.Resource.Organization.OrganizationCreateDTO
import Registry.Api.Resource.Package.PackageSimpleDTO
import Registry.Api.Resource.Template.TemplateSimpleDTO
import Shared.Api.Resource.Organization.OrganizationSimpleDTO
import Wizard.Api.Resource.Registry.RegistryCreateDTO
import Wizard.Model.Config.AppConfig
import Wizard.Model.Registry.RegistryOrganization
import Wizard.Model.Registry.RegistryPackage
import Wizard.Model.Registry.RegistryTemplate

toOrganizationCreate :: AppConfig -> RegistryCreateDTO -> OrganizationCreateDTO
toOrganizationCreate appConfig reqDto =
  OrganizationCreateDTO
    { _organizationCreateDTOOrganizationId = appConfig ^. organization . organizationId
    , _organizationCreateDTOName = appConfig ^. organization . name
    , _organizationCreateDTODescription = appConfig ^. organization . description
    , _organizationCreateDTOEmail = reqDto ^. email
    }

toRegistryOrganization :: OrganizationSimpleDTO -> UTCTime -> RegistryOrganization
toRegistryOrganization dto now =
  RegistryOrganization
    { _registryOrganizationOrganizationId = dto ^. organizationId
    , _registryOrganizationName = dto ^. name
    , _registryOrganizationLogo = dto ^. logo
    , _registryOrganizationCreatedAt = now
    }

toRegistryPackage :: PackageSimpleDTO -> UTCTime -> RegistryPackage
toRegistryPackage dto now =
  RegistryPackage
    { _registryPackageOrganizationId = dto ^. organizationId
    , _registryPackageKmId = dto ^. kmId
    , _registryPackageRemoteVersion = dto ^. version
    , _registryPackageCreatedAt = now
    }

toRegistryTemplate :: TemplateSimpleDTO -> UTCTime -> RegistryTemplate
toRegistryTemplate dto now =
  RegistryTemplate
    { _registryTemplateOrganizationId = dto ^. organizationId
    , _registryTemplateTemplateId = dto ^. templateId
    , _registryTemplateRemoteVersion = dto ^. version
    , _registryTemplateCreatedAt = now
    }
