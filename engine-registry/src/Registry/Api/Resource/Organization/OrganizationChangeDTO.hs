module Registry.Api.Resource.Organization.OrganizationChangeDTO where

import GHC.Generics

data OrganizationChangeDTO = OrganizationChangeDTO
  { name :: String
  , description :: String
  , email :: String
  }
  deriving (Show, Eq, Generic)
