module Registry.Database.Migration.Development.Organization.Data.Organizations where

import Data.Maybe (fromJust)
import Data.Time

import Registry.Api.Resource.Organization.OrganizationChangeDTO
import Registry.Api.Resource.Organization.OrganizationCreateDTO
import Registry.Api.Resource.Organization.OrganizationDTO
import Registry.Api.Resource.Organization.OrganizationStateDTO
import Registry.Model.Organization.Organization
import Registry.Service.Organization.OrganizationMapper
import Shared.Database.Migration.Development.Organization.Data.Organizations

orgGlobal :: Organization
orgGlobal =
  Organization
    { organizationId = "global"
    , name = "Organization"
    , description = "Some description of Organization"
    , email = "organization@example.com"
    , oRole = AdminRole
    , token = "GlobalToken"
    , active = True
    , logo = Just orgLogo
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

orgGlobalDTO :: OrganizationDTO
orgGlobalDTO = toDTO orgGlobal

orgGlobalCreate :: OrganizationCreateDTO
orgGlobalCreate =
  OrganizationCreateDTO
    { organizationId = orgGlobal.organizationId
    , name = orgGlobal.name
    , description = orgGlobal.description
    , email = orgGlobal.email
    }

orgGlobalEdited :: Organization
orgGlobalEdited =
  Organization
    { organizationId = orgGlobal.organizationId
    , name = "EDITED: Organization"
    , description = "EDITED: Some description of Organization"
    , email = "edited-organization@example.com"
    , oRole = orgGlobal.oRole
    , token = orgGlobal.token
    , active = orgGlobal.active
    , logo = orgGlobal.logo
    , createdAt = orgGlobal.createdAt
    , updatedAt = orgGlobal.updatedAt
    }

orgGlobalEditedChange :: OrganizationChangeDTO
orgGlobalEditedChange =
  OrganizationChangeDTO
    { name = orgGlobalEdited.name
    , description = orgGlobalEdited.description
    , email = orgGlobalEdited.email
    }

orgNetherlands :: Organization
orgNetherlands =
  Organization
    { organizationId = "org.nl"
    , name = "Organization Netherlands"
    , description = "Some description of Organization Netherlands"
    , email = "netherlands@example.com"
    , oRole = UserRole
    , token = "NetherlandsToken"
    , active = True
    , logo = Just orgLogo
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

orgStateDto :: OrganizationStateDTO
orgStateDto = OrganizationStateDTO {active = True}
