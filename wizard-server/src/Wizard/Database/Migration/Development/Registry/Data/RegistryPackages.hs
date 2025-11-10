module Wizard.Database.Migration.Development.Registry.Data.RegistryPackages where

import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Wizard.Model.Registry.RegistryPackage

globalRegistryPackage :: RegistryPackage
globalRegistryPackage =
  RegistryPackage
    { organizationId = globalKmPackage.organizationId
    , kmId = globalKmPackage.kmId
    , remoteVersion = globalKmPackage.version
    , createdAt = globalKmPackage.createdAt
    }

nlRegistryPackage :: RegistryPackage
nlRegistryPackage =
  RegistryPackage
    { organizationId = netherlandsKmPackageV2.organizationId
    , kmId = netherlandsKmPackageV2.kmId
    , remoteVersion = netherlandsKmPackageV2.version
    , createdAt = netherlandsKmPackageV2.createdAt
    }
