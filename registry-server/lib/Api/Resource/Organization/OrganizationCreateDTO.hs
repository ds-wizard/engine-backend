module Api.Resource.Organization.OrganizationCreateDTO where

import GHC.Generics

data OrganizationCreateDTO = OrganizationCreateDTO
  { _organizationCreateDTOOrganizationId :: String
  , _organizationCreateDTOName :: String
  , _organizationCreateDTODescription :: String
  , _organizationCreateDTOEmail :: String
  } deriving (Show, Eq, Generic)
