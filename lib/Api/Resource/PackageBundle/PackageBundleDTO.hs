module Api.Resource.PackageBundle.PackageBundleDTO where

import Api.Resource.Package.PackageDTO
import GHC.Generics

data PackageBundleDTO = PackageBundleDTO
  { _packageBundleDTOBundleId :: String
  , _packageBundleDTOName :: String
  , _packageBundleDTOOrganizationId :: String
  , _packageBundleDTOKmId :: String
  , _packageBundleDTOVersion :: String
  , _packageBundleDTOMetamodelVersion :: Int
  , _packageBundleDTOPackages :: [PackageDTO]
  } deriving (Show, Eq, Generic)
