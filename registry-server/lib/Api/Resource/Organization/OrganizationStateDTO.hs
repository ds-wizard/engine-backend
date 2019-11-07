module Api.Resource.Organization.OrganizationStateDTO where

import GHC.Generics

data OrganizationStateDTO = OrganizationStateDTO
  { _organizationStateDTOActive :: Bool
  } deriving (Show, Eq, Generic)
