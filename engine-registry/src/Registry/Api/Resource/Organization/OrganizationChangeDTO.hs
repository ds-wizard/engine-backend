module Registry.Api.Resource.Organization.OrganizationChangeDTO where

import GHC.Generics

data OrganizationChangeDTO =
  OrganizationChangeDTO
    { _organizationChangeDTOName :: String
    , _organizationChangeDTODescription :: String
    , _organizationChangeDTOEmail :: String
    }
  deriving (Show, Eq, Generic)
