module Integration.Resource.Organization.OrganizationSimpleIDTO where

import GHC.Generics

data OrganizationSimpleIDTO = OrganizationSimpleIDTO
  { _organizationSimpleIDTOOrganizationId :: String
  , _organizationSimpleIDTOName :: String
  , _organizationSimpleIDTOLogo :: Maybe String
  } deriving (Show, Eq, Generic)
