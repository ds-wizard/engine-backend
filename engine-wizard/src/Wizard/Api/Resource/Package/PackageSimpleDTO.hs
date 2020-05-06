module Wizard.Api.Resource.Package.PackageSimpleDTO where

import Data.Time
import GHC.Generics

import Shared.Api.Resource.Organization.OrganizationSimpleDTO
import Wizard.Model.Package.PackageState

data PackageSimpleDTO =
  PackageSimpleDTO
    { _packageSimpleDTOPId :: String
    , _packageSimpleDTOName :: String
    , _packageSimpleDTOOrganizationId :: String
    , _packageSimpleDTOKmId :: String
    , _packageSimpleDTOVersion :: String
    , _packageSimpleDTOVersions :: [String]
    , _packageSimpleDTODescription :: String
    , _packageSimpleDTOState :: PackageState
    , _packageSimpleDTOOrganization :: Maybe OrganizationSimpleDTO
    , _packageSimpleDTOCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)
