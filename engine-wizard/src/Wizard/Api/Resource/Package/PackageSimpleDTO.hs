module Wizard.Api.Resource.Package.PackageSimpleDTO where

import Data.Time
import GHC.Generics

import Wizard.Model.Package.PackageState
import Wizard.Model.Registry.RegistryOrganization

data PackageSimpleDTO =
  PackageSimpleDTO
    { _packageSimpleDTOPId :: String
    , _packageSimpleDTOName :: String
    , _packageSimpleDTOOrganizationId :: String
    , _packageSimpleDTOKmId :: String
    , _packageSimpleDTOVersion :: String
    , _packageSimpleDTORemoteLatestVersion :: Maybe String
    , _packageSimpleDTODescription :: String
    , _packageSimpleDTOState :: PackageState
    , _packageSimpleDTOOrganization :: Maybe RegistryOrganization
    , _packageSimpleDTOCreatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)
