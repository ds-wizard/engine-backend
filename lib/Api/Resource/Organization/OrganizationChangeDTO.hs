module Api.Resource.Organization.OrganizationChangeDTO where

import Data.UUID
import GHC.Generics

data OrganizationChangeDTO = OrganizationChangeDTO
  { _organizationChangeDTOUuid :: UUID
  , _organizationChangeDTOName :: String
  , _organizationChangeDTOOrganizationId :: String
  } deriving (Show, Eq, Generic)
