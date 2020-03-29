module Wizard.Database.Migration.Development.Organization.Data.Organizations where

import Wizard.Api.Resource.Organization.OrganizationSimpleDTO

orgGlobal :: OrganizationSimpleDTO
orgGlobal =
  OrganizationSimpleDTO
    { _organizationSimpleDTOName = "Organization"
    , _organizationSimpleDTOOrganizationId = "global"
    , _organizationSimpleDTOLogo = Nothing
    }

orgNetherlands :: OrganizationSimpleDTO
orgNetherlands =
  OrganizationSimpleDTO
    { _organizationSimpleDTOName = "Organization Netherlands"
    , _organizationSimpleDTOOrganizationId = "org.nl"
    , _organizationSimpleDTOLogo = Nothing
    }
