module Shared.Service.Package.PackageMapper where

import Control.Lens ((^.))

import LensesConfig
import Shared.Api.Resource.Package.PackageDTO
import Shared.Model.Package.Package
import Shared.Model.Package.PackageWithEvents

toPackage :: PackageWithEvents -> Package
toPackage pkg =
  Package
    { _packagePId = pkg ^. pId
    , _packageName = pkg ^. name
    , _packageOrganizationId = pkg ^. organizationId
    , _packageKmId = pkg ^. kmId
    , _packageVersion = pkg ^. version
    , _packageMetamodelVersion = pkg ^. metamodelVersion
    , _packageDescription = pkg ^. description
    , _packageReadme = pkg ^. readme
    , _packageLicense = pkg ^. license
    , _packagePreviousPackageId = pkg ^. previousPackageId
    , _packageForkOfPackageId = pkg ^. forkOfPackageId
    , _packageMergeCheckpointPackageId = pkg ^. mergeCheckpointPackageId
    , _packageCreatedAt = pkg ^. createdAt
    }

toDTO :: PackageWithEvents -> PackageDTO
toDTO pkg =
  PackageDTO
    { _packageDTOPId = pkg ^. pId
    , _packageDTOName = pkg ^. name
    , _packageDTOOrganizationId = pkg ^. organizationId
    , _packageDTOKmId = pkg ^. kmId
    , _packageDTOVersion = pkg ^. version
    , _packageDTOMetamodelVersion = pkg ^. metamodelVersion
    , _packageDTODescription = pkg ^. description
    , _packageDTOReadme = pkg ^. readme
    , _packageDTOLicense = pkg ^. license
    , _packageDTOPreviousPackageId = pkg ^. previousPackageId
    , _packageDTOForkOfPackageId = pkg ^. forkOfPackageId
    , _packageDTOMergeCheckpointPackageId = pkg ^. mergeCheckpointPackageId
    , _packageDTOEvents = pkg ^. events
    , _packageDTOCreatedAt = pkg ^. createdAt
    }

buildPackageId :: String -> String -> String -> String
buildPackageId pkgOrganizationId pkgKmId pkgVersion = pkgOrganizationId ++ ":" ++ pkgKmId ++ ":" ++ pkgVersion
