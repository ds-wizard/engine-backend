module Wizard.Database.Migration.Development.Registry.Data.RegistryOrganizations where

import Shared.Common.Util.Date
import Wizard.Model.Registry.RegistryOrganization
import WizardLib.Common.Api.Resource.Organization.OrganizationSimpleDTO
import WizardLib.Common.Database.Migration.Development.Organization.Data.Organizations

globalRegistryOrganization :: RegistryOrganization
globalRegistryOrganization =
  RegistryOrganization
    { organizationId = orgGlobalSimple.organizationId
    , name = orgGlobalSimple.name
    , logo = orgGlobalSimple.logo
    , createdAt = dt' 2018 1 21
    }

nlRegistryOrganization :: RegistryOrganization
nlRegistryOrganization =
  RegistryOrganization
    { organizationId = orgNetherlandsSimple.organizationId
    , name = orgNetherlandsSimple.name
    , logo = orgNetherlandsSimple.logo
    , createdAt = dt' 2018 1 21
    }
