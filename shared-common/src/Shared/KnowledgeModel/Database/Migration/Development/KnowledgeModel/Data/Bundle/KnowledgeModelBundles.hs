module Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Bundle.KnowledgeModelBundles where

import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.KnowledgeModel.Model.KnowledgeModel.Bundle.KnowledgeModelBundle
import Shared.KnowledgeModel.Model.KnowledgeModel.Bundle.KnowledgeModelBundlePackage
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.KnowledgeModel.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper

netherlandsV2KmBundle :: KnowledgeModelBundle
netherlandsV2KmBundle =
  KnowledgeModelBundle
    { bundleId = netherlandsKmPackageV2.pId
    , name = netherlandsKmPackageV2.name
    , organizationId = netherlandsKmPackageV2.organizationId
    , kmId = netherlandsKmPackageV2.kmId
    , version = netherlandsKmPackageV2.version
    , metamodelVersion = netherlandsKmPackageV2.metamodelVersion
    , packages = [globalKmBundlePackage, netherlandsKmBundlePackage, netherlandsV2KmBundlePackage]
    }

globalKmBundlePackage :: KnowledgeModelBundlePackage
globalKmBundlePackage = toKnowledgeModelBundlePackage globalKmPackage globalKmPackageEvents

netherlandsKmBundlePackage :: KnowledgeModelBundlePackage
netherlandsKmBundlePackage = toKnowledgeModelBundlePackage netherlandsKmPackage netherlandsKmPackageEvents

netherlandsV2KmBundlePackage :: KnowledgeModelBundlePackage
netherlandsV2KmBundlePackage = toKnowledgeModelBundlePackage netherlandsKmPackageV2 netherlandsKmPackageV2Events
