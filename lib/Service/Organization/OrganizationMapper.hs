module Service.Organization.OrganizationMapper where

import Control.Lens ((^.))
import Data.Time

import Api.Resource.Organization.OrganizationChangeDTO
import Api.Resource.Organization.OrganizationDTO
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

fromDTO :: OrganizationChangeDTO -> UTCTime -> UTCTime -> Organization
fromDTO dto orgCreatedAt orgUpdatedAt =
  Organization
  { _organizationUuid = dto ^. uuid
  , _organizationName = dto ^. name
  , _organizationOrganizationId = dto ^. organizationId
  , _organizationCreatedAt = orgCreatedAt
  , _organizationUpdatedAt = orgUpdatedAt
  }
