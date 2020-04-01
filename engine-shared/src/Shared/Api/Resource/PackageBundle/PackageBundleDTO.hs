module Shared.Api.Resource.PackageBundle.PackageBundleDTO where

import GHC.Generics
import Shared.Api.Resource.Package.PackageDTO

data PackageBundleDTO =
  PackageBundleDTO
    { _packageBundleDTOBundleId :: String
    , _packageBundleDTOName :: String
    , _packageBundleDTOOrganizationId :: String
    , _packageBundleDTOKmId :: String
    , _packageBundleDTOVersion :: String
    , _packageBundleDTOMetamodelVersion :: Int
    , _packageBundleDTOPackages :: [PackageDTO]
    }
  deriving (Show, Eq, Generic)
