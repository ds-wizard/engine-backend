module Registry.Service.Organization.OrganizationMapper where

import Data.Time

import Registry.Api.Resource.Organization.OrganizationChangeDTO
import Registry.Api.Resource.Organization.OrganizationCreateDTO
import Registry.Api.Resource.Organization.OrganizationDTO
import Registry.Model.Organization.Organization
import Shared.Api.Resource.Organization.OrganizationSimpleDTO

toDTO :: Organization -> OrganizationDTO
toDTO organization =
  OrganizationDTO
    { organizationId = organization.organizationId
    , name = organization.name
    , description = organization.description
    , email = organization.email
    , oRole = organization.oRole
    , token = organization.token
    , logo = organization.logo
    , active = organization.active
    , createdAt = organization.createdAt
    , updatedAt = organization.updatedAt
    }

toSimpleDTO :: Organization -> OrganizationSimpleDTO
toSimpleDTO organization =
  OrganizationSimpleDTO
    { organizationId = organization.organizationId
    , name = organization.name
    , logo = organization.logo
    }

organizationDTOtoSimpleDTO :: OrganizationDTO -> OrganizationSimpleDTO
organizationDTOtoSimpleDTO organization =
  OrganizationSimpleDTO
    { organizationId = organization.organizationId
    , name = organization.name
    , logo = organization.logo
    }

fromCreateDTO :: OrganizationCreateDTO -> OrganizationRole -> String -> UTCTime -> UTCTime -> UTCTime -> Organization
fromCreateDTO dto orgRole orgToken orgCreatedAt orgUpdatedAt orgLastAccessAt =
  Organization
    { organizationId = dto.organizationId
    , name = dto.name
    , description = dto.description
    , email = dto.email
    , oRole = orgRole
    , token = orgToken
    , active = False
    , logo = Nothing
    , createdAt = orgCreatedAt
    , updatedAt = orgUpdatedAt
    }

fromChangeDTO :: OrganizationChangeDTO -> OrganizationDTO -> UTCTime -> Organization
fromChangeDTO dto org orgUpdatedAt =
  Organization
    { organizationId = org.organizationId
    , name = dto.name
    , description = dto.description
    , email = dto.email
    , oRole = org.oRole
    , token = org.token
    , active = org.active
    , logo = org.logo
    , createdAt = org.createdAt
    , updatedAt = orgUpdatedAt
    }
