module Api.Resource.Organization.OrganizationSimpleDTO where

import GHC.Generics

data OrganizationSimpleDTO = OrganizationSimpleDTO
  { _organizationSimpleDTOName :: String
  , _organizationSimpleDTOOrganizationId :: String
  , _organizationSimpleDTOLogo :: Maybe String
  } deriving (Show, Eq, Generic)
