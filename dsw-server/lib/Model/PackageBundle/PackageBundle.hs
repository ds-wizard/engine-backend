module Model.PackageBundle.PackageBundle where

import GHC.Generics

import Model.Package.PackageWithEvents

data PackageBundle = PackageBundle
  { _packageBundleBundleId :: String
  , _packageBundleName :: String
  , _packageBundleOrganizationId :: String
  , _packageBundleKmId :: String
  , _packageBundleVersion :: String
  , _packageBundleMetamodelVersion :: Int
  , _packageBundlePackages :: [PackageWithEvents]
  } deriving (Show, Eq, Generic)
