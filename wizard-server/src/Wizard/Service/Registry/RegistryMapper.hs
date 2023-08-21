module Wizard.Service.Registry.RegistryMapper where

import Data.Time

import RegistryLib.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO
import RegistryLib.Api.Resource.Locale.LocaleDTO
import RegistryLib.Api.Resource.Organization.OrganizationCreateDTO
import RegistryLib.Api.Resource.Package.PackageSimpleDTO
import Wizard.Api.Resource.Registry.RegistryCreateDTO
import Wizard.Model.Config.AppConfig
import Wizard.Model.Registry.RegistryLocale
import Wizard.Model.Registry.RegistryOrganization
import Wizard.Model.Registry.RegistryPackage
import Wizard.Model.Registry.RegistryTemplate
import WizardLib.Common.Api.Resource.Organization.OrganizationSimpleDTO

toOrganizationCreate :: AppConfig -> RegistryCreateDTO -> OrganizationCreateDTO
toOrganizationCreate appConfig reqDto =
  OrganizationCreateDTO
    { organizationId = appConfig.organization.organizationId
    , name = appConfig.organization.name
    , description = appConfig.organization.description
    , email = reqDto.email
    }

toRegistryOrganization :: OrganizationSimpleDTO -> UTCTime -> RegistryOrganization
toRegistryOrganization dto now =
  RegistryOrganization
    { organizationId = dto.organizationId
    , name = dto.name
    , logo = dto.logo
    , createdAt = now
    }

toRegistryPackage :: PackageSimpleDTO -> UTCTime -> RegistryPackage
toRegistryPackage dto now =
  RegistryPackage
    { organizationId = dto.organizationId
    , kmId = dto.kmId
    , remoteVersion = dto.version
    , createdAt = now
    }

toRegistryTemplate :: DocumentTemplateSimpleDTO -> UTCTime -> RegistryTemplate
toRegistryTemplate dto now =
  RegistryTemplate
    { organizationId = dto.organizationId
    , templateId = dto.templateId
    , remoteVersion = dto.version
    , createdAt = now
    }

toRegistryLocale :: LocaleDTO -> UTCTime -> RegistryLocale
toRegistryLocale dto now =
  RegistryLocale
    { organizationId = dto.organizationId
    , localeId = dto.localeId
    , remoteVersion = dto.version
    , createdAt = now
    }
