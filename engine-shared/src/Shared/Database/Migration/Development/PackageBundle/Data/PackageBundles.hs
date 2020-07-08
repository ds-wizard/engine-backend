module Shared.Database.Migration.Development.PackageBundle.Data.PackageBundles where

import Control.Lens ((^.))

import LensesConfig
import Shared.Database.Migration.Development.Package.Data.Packages
import Shared.Model.PackageBundle.PackageBundle

netherlandsPackageBudle :: PackageBundle
netherlandsPackageBudle =
  PackageBundle
    { _packageBundleBundleId = netherlandsPackage ^. pId
    , _packageBundleName = netherlandsPackage ^. name
    , _packageBundleOrganizationId = netherlandsPackage ^. organizationId
    , _packageBundleKmId = netherlandsPackage ^. kmId
    , _packageBundleVersion = netherlandsPackage ^. version
    , _packageBundleMetamodelVersion = netherlandsPackage ^. metamodelVersion
    , _packageBundlePackages = [globalPackage, netherlandsPackage]
    }

netherlandsPackageV2Budle :: PackageBundle
netherlandsPackageV2Budle =
  PackageBundle
    { _packageBundleBundleId = netherlandsPackageV2 ^. pId
    , _packageBundleName = netherlandsPackageV2 ^. name
    , _packageBundleOrganizationId = netherlandsPackageV2 ^. organizationId
    , _packageBundleKmId = netherlandsPackageV2 ^. kmId
    , _packageBundleVersion = netherlandsPackageV2 ^. version
    , _packageBundleMetamodelVersion = netherlandsPackageV2 ^. metamodelVersion
    , _packageBundlePackages = [globalPackage, netherlandsPackage, netherlandsPackageV2]
    }

germanyBundle :: PackageBundle
germanyBundle =
  PackageBundle
    { _packageBundleBundleId = germanyPackage ^. pId
    , _packageBundleName = germanyPackage ^. name
    , _packageBundleOrganizationId = germanyPackage ^. organizationId
    , _packageBundleKmId = germanyPackage ^. kmId
    , _packageBundleVersion = germanyPackage ^. version
    , _packageBundleMetamodelVersion = germanyPackage ^. metamodelVersion
    , _packageBundlePackages = [globalPackageEmpty, germanyPackage]
    }
