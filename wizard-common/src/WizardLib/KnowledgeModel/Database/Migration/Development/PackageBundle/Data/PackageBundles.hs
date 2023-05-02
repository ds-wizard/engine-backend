module WizardLib.KnowledgeModel.Database.Migration.Development.PackageBundle.Data.PackageBundles where

import WizardLib.KnowledgeModel.Database.Migration.Development.Package.Data.Packages
import WizardLib.KnowledgeModel.Model.Package.PackageWithEvents
import WizardLib.KnowledgeModel.Model.PackageBundle.PackageBundle

netherlandsPackageBudle :: PackageBundle
netherlandsPackageBudle =
  PackageBundle
    { bundleId = netherlandsPackage.pId
    , name = netherlandsPackage.name
    , organizationId = netherlandsPackage.organizationId
    , kmId = netherlandsPackage.kmId
    , version = netherlandsPackage.version
    , metamodelVersion = netherlandsPackage.metamodelVersion
    , packages = [globalPackage, netherlandsPackage]
    }

netherlandsPackageV2Budle :: PackageBundle
netherlandsPackageV2Budle =
  PackageBundle
    { bundleId = netherlandsPackageV2.pId
    , name = netherlandsPackageV2.name
    , organizationId = netherlandsPackageV2.organizationId
    , kmId = netherlandsPackageV2.kmId
    , version = netherlandsPackageV2.version
    , metamodelVersion = netherlandsPackageV2.metamodelVersion
    , packages = [globalPackage, netherlandsPackage, netherlandsPackageV2]
    }

germanyBundle :: PackageBundle
germanyBundle =
  PackageBundle
    { bundleId = germanyPackage.pId
    , name = germanyPackage.name
    , organizationId = germanyPackage.organizationId
    , kmId = germanyPackage.kmId
    , version = germanyPackage.version
    , metamodelVersion = germanyPackage.metamodelVersion
    , packages = [globalPackageEmpty, germanyPackage]
    }
