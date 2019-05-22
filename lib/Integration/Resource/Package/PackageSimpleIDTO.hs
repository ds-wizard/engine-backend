module Integration.Resource.Package.PackageSimpleIDTO where

import Data.Time

import Integration.Resource.Organization.OrganizationSimpleIDTO

data PackageSimpleIDTO = PackageSimpleIDTO
  { _packageSimpleIDTOPId :: String
  , _packageSimpleIDTOName :: String
  , _packageSimpleIDTOOrganizationId :: String
  , _packageSimpleIDTOKmId :: String
  , _packageSimpleIDTOVersion :: String
  , _packageSimpleIDTODescription :: String
  , _packageSimpleIDTOOrganization :: OrganizationSimpleIDTO
  , _packageSimpleIDTOCreatedAt :: UTCTime
  } deriving (Show, Eq)
