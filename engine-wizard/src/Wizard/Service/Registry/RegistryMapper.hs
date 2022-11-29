module Wizard.Service.Registry.RegistryMapper where

import Data.Time

import Registry.Api.Resource.Locale.LocaleDTO
import Registry.Api.Resource.Organization.OrganizationCreateDTO
import Registry.Api.Resource.Package.PackageSimpleDTO
import Registry.Api.Resource.Template.TemplateSimpleDTO
import Shared.Api.Resource.Organization.OrganizationSimpleDTO
import Wizard.Api.Resource.Registry.RegistryCreateDTO
import Wizard.Model.Config.AppConfig
import Wizard.Model.Registry.RegistryLocale
import Wizard.Model.Registry.RegistryOrganization
import Wizard.Model.Registry.RegistryPackage
import Wizard.Model.Registry.RegistryTemplate

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

toRegistryTemplate :: TemplateSimpleDTO -> UTCTime -> RegistryTemplate
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
