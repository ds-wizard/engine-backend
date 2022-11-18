module Wizard.Database.Migration.Development.Registry.Data.RegistryPackages where

import Shared.Database.Migration.Development.Package.Data.Packages
import Shared.Model.Package.PackageWithEvents
import Wizard.Model.Registry.RegistryPackage

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
