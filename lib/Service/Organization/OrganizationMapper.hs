module Service.Organization.OrganizationMapper where

import Control.Lens ((^.))
import Data.Aeson
import Data.UUID (UUID)

import Api.Resource.Organization.OrganizationDTO
import Common.Types
import LensesConfig
import Model.Organization.Organization

toDTO :: Organization -> OrganizationDTO
toDTO organization =
  OrganizationDTO
  { _organizationDTOUuid = organization ^. uuid
  , _organizationDTOName = organization ^. name
  , _organizationDTOOrganizationId = organization ^. organizationId
  , _organizationDTOCreatedAt = organization ^. createdAt
  , _organizationDTOUpdatedAt = organization ^. updatedAt
  }

fromDTO :: OrganizationDTO -> Organization
fromDTO dto =
  Organization
  { _organizationUuid = dto ^. uuid
  , _organizationName = dto ^. name
  , _organizationOrganizationId = dto ^. organizationId
  , _organizationCreatedAt = dto ^. createdAt
  , _organizationUpdatedAt = dto ^. updatedAt
  }
