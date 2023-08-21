module RegistryLib.Service.Organization.OrganizationMapper where

import RegistryLib.Api.Resource.Organization.OrganizationDTO
import RegistryLib.Model.Organization.Organization

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
