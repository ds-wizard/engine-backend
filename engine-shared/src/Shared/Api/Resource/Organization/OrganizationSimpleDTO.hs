module Shared.Api.Resource.Organization.OrganizationSimpleDTO where

import GHC.Generics

data OrganizationSimpleDTO =
  OrganizationSimpleDTO
    { _organizationSimpleDTOOrganizationId :: String
    , _organizationSimpleDTOName :: String
    , _organizationSimpleDTOLogo :: Maybe String
    }
  deriving (Show, Eq, Generic)
