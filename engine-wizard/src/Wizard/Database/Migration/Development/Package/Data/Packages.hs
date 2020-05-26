module Wizard.Database.Migration.Development.Package.Data.Packages where

import Control.Lens ((^.))

import LensesConfig
import Registry.Api.Resource.Package.PackageSimpleDTO
import Shared.Api.Resource.Organization.OrganizationSimpleDTO
import Shared.Database.Migration.Development.Organization.Data.Organizations
import Shared.Database.Migration.Development.Package.Data.Packages

globalRemotePackage :: PackageSimpleDTO
globalRemotePackage =
  PackageSimpleDTO
    { _packageSimpleDTOPId = globalPackage ^. pId
    , _packageSimpleDTOName = globalPackage ^. name
    , _packageSimpleDTOOrganizationId = globalPackage ^. organizationId
    , _packageSimpleDTOKmId = globalPackage ^. kmId
    , _packageSimpleDTOVersion = globalPackage ^. version
    , _packageSimpleDTODescription = globalPackage ^. description
    , _packageSimpleDTOOrganization =
        OrganizationSimpleDTO
          { _organizationSimpleDTOOrganizationId = orgGlobalSimple ^. organizationId
          , _organizationSimpleDTOName = orgGlobalSimple ^. name
          , _organizationSimpleDTOLogo = Just orgLogo
          }
    , _packageSimpleDTOCreatedAt = globalPackage ^. createdAt
    }

globalNetherlandsPackage :: PackageSimpleDTO
globalNetherlandsPackage =
  PackageSimpleDTO
    { _packageSimpleDTOPId = netherlandsPackageV2 ^. pId
    , _packageSimpleDTOName = netherlandsPackageV2 ^. name
    , _packageSimpleDTOOrganizationId = netherlandsPackageV2 ^. organizationId
    , _packageSimpleDTOKmId = netherlandsPackageV2 ^. kmId
    , _packageSimpleDTOVersion = netherlandsPackageV2 ^. version
    , _packageSimpleDTODescription = netherlandsPackageV2 ^. description
    , _packageSimpleDTOOrganization =
        OrganizationSimpleDTO
          { _organizationSimpleDTOOrganizationId = orgNetherlandsSimple ^. organizationId
          , _organizationSimpleDTOName = orgNetherlandsSimple ^. name
          , _organizationSimpleDTOLogo = Just orgLogo
          }
    , _packageSimpleDTOCreatedAt = globalPackage ^. createdAt
    }
