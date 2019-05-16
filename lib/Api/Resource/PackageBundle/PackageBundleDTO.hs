module Api.Resource.PackageBundle.PackageBundleDTO where

import Api.Resource.Package.PackageWithEventsDTO

data PackageBundleDTO = PackageBundleDTO
  { _packageBundleDTOBundleId :: String
  , _packageBundleDTOName :: String
  , _packageBundleDTOOrganizationId :: String
  , _packageBundleDTOKmId :: String
  , _packageBundleDTOVersion :: String
  , _packageBundleDTOMetamodelVersion :: Int
  , _packageBundleDTOPackages :: [PackageWithEventsDTO]
  } deriving (Show, Eq)
