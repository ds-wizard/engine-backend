module Service.Package.PackageMapper where

import Control.Lens ((^.))

import Api.Resource.Package.PackageDTO
import Api.Resource.Package.PackageDetailDTO
import Api.Resource.Package.PackageSimpleDTO
import LensesConfig
import Model.Organization.Organization
import Model.Package.Package
import Model.Package.PackageWithEvents
import Service.Event.EventMapper
import qualified Service.Organization.OrganizationMapper as OM

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
  , _packageDTOEvents = toDTOs (pkg ^. events)
  , _packageDTOCreatedAt = pkg ^. createdAt
  }

toSimpleDTO :: Package -> Organization -> PackageSimpleDTO
toSimpleDTO pkg org =
  PackageSimpleDTO
  { _packageSimpleDTOPId = pkg ^. pId
  , _packageSimpleDTOName = pkg ^. name
  , _packageSimpleDTOOrganizationId = pkg ^. organizationId
  , _packageSimpleDTOKmId = pkg ^. kmId
  , _packageSimpleDTOVersion = pkg ^. version
  , _packageSimpleDTODescription = pkg ^. description
  , _packageSimpleDTOCreatedAt = pkg ^. createdAt
  , _packageSimpleDTOOrganization = OM.toSimpleDTO org
  }

toDetailDTO :: Package -> [String] -> Organization -> PackageDetailDTO
toDetailDTO pkg versions org =
  PackageDetailDTO
  { _packageDetailDTOPId = pkg ^. pId
  , _packageDetailDTOName = pkg ^. name
  , _packageDetailDTOOrganizationId = pkg ^. organizationId
  , _packageDetailDTOKmId = pkg ^. kmId
  , _packageDetailDTOVersion = pkg ^. version
  , _packageDetailDTODescription = pkg ^. description
  , _packageDetailDTOReadme = pkg ^. readme
  , _packageDetailDTOLicense = pkg ^. license
  , _packageDetailDTOMetamodelVersion = pkg ^. metamodelVersion
  , _packageDetailDTOPreviousPackageId = pkg ^. previousPackageId
  , _packageDetailDTOForkOfPackageId = pkg ^. forkOfPackageId
  , _packageDetailDTOMergeCheckpointPackageId = pkg ^. mergeCheckpointPackageId
  , _packageDetailDTOVersions = versions
  , _packageDetailDTOOrganization = OM.toSimpleDTO org
  , _packageDetailDTOCreatedAt = pkg ^. createdAt
  }

buildPackageId :: String -> String -> String -> String
buildPackageId pkgOrganizationId pkgKmId pkgVersion = pkgOrganizationId ++ ":" ++ pkgKmId ++ ":" ++ pkgVersion
