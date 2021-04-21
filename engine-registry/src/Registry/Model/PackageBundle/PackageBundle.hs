module Registry.Model.PackageBundle.PackageBundle where

import GHC.Generics

import Shared.Model.Package.PackageWithEventsRaw

data PackageBundle =
  PackageBundle
    { _packageBundleBundleId :: String
    , _packageBundleName :: String
    , _packageBundleOrganizationId :: String
    , _packageBundleKmId :: String
    , _packageBundleVersion :: String
    , _packageBundleMetamodelVersion :: Int
    , _packageBundlePackages :: [PackageWithEventsRaw]
    }
  deriving (Show, Eq, Generic)
