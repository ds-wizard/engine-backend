module Api.Resource.Organization.OrganizationDTO where

import Data.Time
import Data.UUID
import GHC.Generics

data OrganizationDTO = OrganizationDTO
  { _organizationDTOUuid :: UUID
  , _organizationDTOName :: String
  , _organizationDTOOrganizationId :: String
  , _organizationDTOCreatedAt :: UTCTime
  , _organizationDTOUpdatedAt :: UTCTime
  } deriving (Show, Generic)

instance Eq OrganizationDTO where
  a == b =
    _organizationDTOUuid a == _organizationDTOUuid b &&
    _organizationDTOName a == _organizationDTOName b &&
    _organizationDTOOrganizationId a == _organizationDTOOrganizationId b
