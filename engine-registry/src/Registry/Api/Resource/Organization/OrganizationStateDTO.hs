module Registry.Api.Resource.Organization.OrganizationStateDTO where

import GHC.Generics

data OrganizationStateDTO = OrganizationStateDTO
  { active :: Bool
  }
  deriving (Show, Eq, Generic)
