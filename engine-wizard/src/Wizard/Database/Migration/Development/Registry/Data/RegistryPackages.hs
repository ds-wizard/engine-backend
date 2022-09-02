module Wizard.Database.Migration.Development.Registry.Data.RegistryPackages where

import Control.Lens ((^.))

import LensesConfig
import Shared.Database.Migration.Development.Package.Data.Packages
import Wizard.Model.Registry.RegistryPackage

globalRegistryPackage :: RegistryPackage
globalRegistryPackage =
  RegistryPackage
    { _registryPackageOrganizationId = globalPackage ^. organizationId
    , _registryPackageKmId = globalPackage ^. kmId
    , _registryPackageRemoteVersion = globalPackage ^. version
    , _registryPackageCreatedAt = globalPackage ^. createdAt
    }

nlRegistryPackage :: RegistryPackage
nlRegistryPackage =
  RegistryPackage
    { _registryPackageOrganizationId = netherlandsPackageV2 ^. organizationId
    , _registryPackageKmId = netherlandsPackageV2 ^. kmId
    , _registryPackageRemoteVersion = netherlandsPackageV2 ^. version
    , _registryPackageCreatedAt = netherlandsPackageV2 ^. createdAt
    }
