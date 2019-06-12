module Database.Migration.Development.PackageBundle.Data.PackageBundles where

import Control.Lens ((^.))

import Database.Migration.Development.Package.Data.Packages
import LensesConfig
import Model.PackageBundle.PackageBundle

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
