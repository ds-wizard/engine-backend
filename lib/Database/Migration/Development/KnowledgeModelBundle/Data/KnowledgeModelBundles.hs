module Database.Migration.Development.KnowledgeModelBundle.Data.KnowledgeModelBundles where

import Control.Lens ((^.))

import Database.Migration.Development.Package.Data.Packages
import LensesConfig
import Model.KnowledgeModelBundle.KnowledgeModelBundle

netherlandsPackageV2KMBudle :: KnowledgeModelBundle
netherlandsPackageV2KMBudle =
  KnowledgeModelBundle
  { _knowledgeModelBundleBundleId = netherlandsPackageV2 ^. pId
  , _knowledgeModelBundleName = netherlandsPackageV2 ^. name
  , _knowledgeModelBundleOrganizationId = netherlandsPackageV2 ^. organizationId
  , _knowledgeModelBundleKmId = netherlandsPackageV2 ^. kmId
  , _knowledgeModelBundleVersion = netherlandsPackageV2 ^. version
  , _knowledgeModelBundlePackages = [globalPackage, netherlandsPackage, netherlandsPackageV2]
  }
