module Registry.Database.Migration.Development.Organization.Data.Organizations where

import Control.Lens ((^.))
import Data.Maybe (fromJust)
import Data.Time

import LensesConfig
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
    { _organizationOrganizationId = "global"
    , _organizationName = "Organization"
    , _organizationDescription = "Some description of Organization"
    , _organizationEmail = "organization@example.com"
    , _organizationRole = AdminRole
    , _organizationToken = "GlobalToken"
    , _organizationActive = True
    , _organizationLogo = Just orgLogo
    , _organizationCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , _organizationUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

orgGlobalDTO :: OrganizationDTO
orgGlobalDTO = toDTO orgGlobal

orgGlobalCreate :: OrganizationCreateDTO
orgGlobalCreate =
  OrganizationCreateDTO
    { _organizationCreateDTOOrganizationId = orgGlobal ^. organizationId
    , _organizationCreateDTOName = orgGlobal ^. name
    , _organizationCreateDTODescription = orgGlobal ^. description
    , _organizationCreateDTOEmail = orgGlobal ^. email
    }

orgGlobalEdited :: Organization
orgGlobalEdited =
  Organization
    { _organizationOrganizationId = orgGlobal ^. organizationId
    , _organizationName = "EDITED: Organization"
    , _organizationDescription = "EDITED: Some description of Organization"
    , _organizationEmail = "edited-organization@example.com"
    , _organizationRole = orgGlobal ^. role
    , _organizationToken = orgGlobal ^. token
    , _organizationActive = orgGlobal ^. active
    , _organizationLogo = orgGlobal ^. logo
    , _organizationCreatedAt = orgGlobal ^. createdAt
    , _organizationUpdatedAt = orgGlobal ^. updatedAt
    }

orgGlobalEditedChange :: OrganizationChangeDTO
orgGlobalEditedChange =
  OrganizationChangeDTO
    { _organizationChangeDTOName = orgGlobalEdited ^. name
    , _organizationChangeDTODescription = orgGlobalEdited ^. description
    , _organizationChangeDTOEmail = orgGlobalEdited ^. email
    }

orgNetherlands :: Organization
orgNetherlands =
  Organization
    { _organizationOrganizationId = "org.nl"
    , _organizationName = "Organization Netherlands"
    , _organizationDescription = "Some description of Organization Netherlands"
    , _organizationEmail = "netherlands@example.com"
    , _organizationRole = UserRole
    , _organizationToken = "NetherlandsToken"
    , _organizationActive = True
    , _organizationLogo = Just orgLogo
    , _organizationCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , _organizationUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
    }

orgStateDto :: OrganizationStateDTO
orgStateDto = OrganizationStateDTO {_organizationStateDTOActive = True}
