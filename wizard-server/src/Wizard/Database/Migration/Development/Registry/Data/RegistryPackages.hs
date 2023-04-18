module Wizard.Database.Migration.Development.Registry.Data.RegistryPackages where

import Wizard.Model.Registry.RegistryPackage
import WizardLib.KnowledgeModel.Database.Migration.Development.Package.Data.Packages
import WizardLib.KnowledgeModel.Model.Package.PackageWithEvents

globalRegistryPackage :: RegistryPackage
globalRegistryPackage =
  RegistryPackage
    { organizationId = globalPackage.organizationId
    , kmId = globalPackage.kmId
    , remoteVersion = globalPackage.version
    , createdAt = globalPackage.createdAt
    }

nlRegistryPackage :: RegistryPackage
nlRegistryPackage =
  RegistryPackage
    { organizationId = netherlandsPackageV2.organizationId
    , kmId = netherlandsPackageV2.kmId
    , remoteVersion = netherlandsPackageV2.version
    , createdAt = netherlandsPackageV2.createdAt
    }
