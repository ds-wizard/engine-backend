module Shared.Database.Migration.Development.Organization.Data.Organizations where

import Shared.Api.Resource.Organization.OrganizationSimpleDTO

orgGlobalSimple :: OrganizationSimpleDTO
orgGlobalSimple =
  OrganizationSimpleDTO
    { _organizationSimpleDTOName = "Organization"
    , _organizationSimpleDTOOrganizationId = "global"
    , _organizationSimpleDTOLogo = Just orgLogo
    }

orgNetherlandsSimple :: OrganizationSimpleDTO
orgNetherlandsSimple =
  OrganizationSimpleDTO
    { _organizationSimpleDTOName = "Organization Netherlands"
    , _organizationSimpleDTOOrganizationId = "org.nl"
    , _organizationSimpleDTOLogo = Just orgLogo
    }

orgLogo :: String
orgLogo =
  "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+P+/HgAFhAJ/wlseKgAAAABJRU5ErkJggg=="
