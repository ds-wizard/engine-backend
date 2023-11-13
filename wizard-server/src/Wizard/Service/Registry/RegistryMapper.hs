module Wizard.Service.Registry.RegistryMapper where

import Data.Time

import RegistryLib.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO
import RegistryLib.Api.Resource.Locale.LocaleDTO
import RegistryLib.Api.Resource.Organization.OrganizationCreateDTO
import RegistryLib.Api.Resource.Package.PackageSimpleDTO
import RegistryLib.Model.Organization.OrganizationSimple
import Wizard.Api.Resource.Registry.RegistryCreateDTO
import Wizard.Model.Registry.RegistryLocale
import Wizard.Model.Registry.RegistryOrganization
import Wizard.Model.Registry.RegistryPackage
import Wizard.Model.Registry.RegistryTemplate
import Wizard.Model.Tenant.Config.TenantConfig

toOrganizationCreate :: TenantConfig -> RegistryCreateDTO -> OrganizationCreateDTO
toOrganizationCreate tenantConfig reqDto =
  OrganizationCreateDTO
    { organizationId = tenantConfig.organization.organizationId
    , name = tenantConfig.organization.name
    , description = tenantConfig.organization.description
    , email = reqDto.email
    }

toRegistryOrganization :: OrganizationSimple -> UTCTime -> RegistryOrganization
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
