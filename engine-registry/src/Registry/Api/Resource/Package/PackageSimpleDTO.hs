module Registry.Api.Resource.Package.PackageSimpleDTO where

import Data.Time
import GHC.Generics

import Shared.Api.Resource.Organization.OrganizationSimpleDTO

data PackageSimpleDTO =
  PackageSimpleDTO
    { _packageSimpleDTOPId :: String
    , _packageSimpleDTOName :: String
    , _packageSimpleDTOOrganizationId :: String
    , _packageSimpleDTOKmId :: String
    , _packageSimpleDTOVersion :: String
    , _packageSimpleDTODescription :: String
    , _packageSimpleDTOOrganization :: OrganizationSimpleDTO
    , _packageSimpleDTOCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)
