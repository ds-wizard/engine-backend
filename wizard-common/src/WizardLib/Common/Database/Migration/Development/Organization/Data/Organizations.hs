module WizardLib.Common.Database.Migration.Development.Organization.Data.Organizations where

import WizardLib.Common.Api.Resource.Organization.OrganizationSimpleDTO

orgGlobalSimple :: OrganizationSimpleDTO
orgGlobalSimple =
  OrganizationSimpleDTO
    { name = "Organization"
    , organizationId = "global"
    , logo = Just orgLogo
    }

orgNetherlandsSimple :: OrganizationSimpleDTO
orgNetherlandsSimple =
  OrganizationSimpleDTO
    { name = "Organization Netherlands"
    , organizationId = "org.nl"
    , logo = Just orgLogo
    }

orgLogo :: String
orgLogo =
  "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+P+/HgAFhAJ/wlseKgAAAABJRU5ErkJggg=="
