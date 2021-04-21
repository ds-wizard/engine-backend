module Registry.Api.Resource.PackageBundle.PackageBundleDTO where

import GHC.Generics
import Registry.Api.Resource.Package.PackageRawDTO

data PackageBundleDTO =
  PackageBundleDTO
    { _packageBundleDTOBundleId :: String
    , _packageBundleDTOName :: String
    , _packageBundleDTOOrganizationId :: String
    , _packageBundleDTOKmId :: String
    , _packageBundleDTOVersion :: String
    , _packageBundleDTOMetamodelVersion :: Int
    , _packageBundleDTOPackages :: [PackageRawDTO]
    }
  deriving (Show, Eq, Generic)
