module Database.Migration.Development.Organization.Data.Organizations where

import Control.Lens ((^.))
import Data.Maybe (fromJust)
import Data.Time

import Api.Resource.Organization.OrganizationChangeDTO
import Api.Resource.Organization.OrganizationCreateDTO
import LensesConfig
import Model.Organization.Organization

orgDsw :: Organization
orgDsw =
  Organization
  { _organizationOrganizationId = "dsw"
  , _organizationName = "DSW"
  , _organizationDescription = "Some description of DSW"
  , _organizationEmail = "dsw@example.com"
  , _organizationRole = AdminRole
  , _organizationToken = "DswToken"
  , _organizationActive = True
  , _organizationLogo = Just orgLogo
  , _organizationCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
  , _organizationUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
  }

orgDswCreate :: OrganizationCreateDTO
orgDswCreate =
  OrganizationCreateDTO
  { _organizationCreateDTOOrganizationId = orgDsw ^. organizationId
  , _organizationCreateDTOName = orgDsw ^. name
  , _organizationCreateDTODescription = orgDsw ^. description
  , _organizationCreateDTOEmail = orgDsw ^. email
  }

editedOrgDsw :: Organization
editedOrgDsw =
  Organization
  { _organizationOrganizationId = orgDsw ^. organizationId
  , _organizationName = "EDITED: DSW"
  , _organizationDescription = "EDITED: Some description of DSW"
  , _organizationEmail = "edited-dsw@example.com"
  , _organizationRole = orgDsw ^. role
  , _organizationToken = orgDsw ^. token
  , _organizationActive = orgDsw ^. active
  , _organizationLogo = orgDsw ^. logo
  , _organizationCreatedAt = orgDsw ^. createdAt
  , _organizationUpdatedAt = orgDsw ^. updatedAt
  }

editedOrgDswChange :: OrganizationChangeDTO
editedOrgDswChange =
  OrganizationChangeDTO
  { _organizationChangeDTOName = editedOrgDsw ^. name
  , _organizationChangeDTODescription = editedOrgDsw ^. description
  , _organizationChangeDTOEmail = editedOrgDsw ^. email
  }

orgNetherlands :: Organization
orgNetherlands =
  Organization
  { _organizationOrganizationId = "dsw.nl"
  , _organizationName = "DSW Netherlands"
  , _organizationDescription = "Some description of DSW Netherlands"
  , _organizationEmail = "netherlands@example.com"
  , _organizationRole = UserRole
  , _organizationToken = "NetherlandsToken"
  , _organizationActive = True
  , _organizationLogo = Just orgLogo
  , _organizationCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
  , _organizationUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
  }

orgLogo :: String
orgLogo =
  "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+P+/HgAFhAJ/wlseKgAAAABJRU5ErkJggg=="
