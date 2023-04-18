module Registry.Api.Resource.Organization.OrganizationCreateDTO where

import GHC.Generics

data OrganizationCreateDTO = OrganizationCreateDTO
  { organizationId :: String
  , name :: String
  , description :: String
  , email :: String
  }
  deriving (Show, Eq, Generic)
