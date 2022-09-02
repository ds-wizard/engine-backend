module Wizard.Database.Migration.Development.Registry.Data.RegistryOrganizations where

import Control.Lens ((^.))

import LensesConfig
import Shared.Database.Migration.Development.Organization.Data.Organizations
import Shared.Util.Date
import Wizard.Model.Registry.RegistryOrganization

globalRegistryOrganization :: RegistryOrganization
globalRegistryOrganization =
  RegistryOrganization
    { _registryOrganizationOrganizationId = orgGlobalSimple ^. organizationId
    , _registryOrganizationName = orgGlobalSimple ^. name
    , _registryOrganizationLogo = orgGlobalSimple ^. logo
    , _registryOrganizationCreatedAt = dt' 2018 1 21
    }

nlRegistryOrganization :: RegistryOrganization
nlRegistryOrganization =
  RegistryOrganization
    { _registryOrganizationOrganizationId = orgNetherlandsSimple ^. organizationId
    , _registryOrganizationName = orgNetherlandsSimple ^. name
    , _registryOrganizationLogo = orgNetherlandsSimple ^. logo
    , _registryOrganizationCreatedAt = dt' 2018 1 21
    }
