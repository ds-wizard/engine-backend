module Registry.Service.Package.PackageMapper where

import Control.Lens ((^.))

import LensesConfig
import Registry.Api.Resource.Package.PackageDetailDTO
import Registry.Api.Resource.Package.PackageSimpleDTO
import Registry.Model.Organization.Organization
import qualified Registry.Service.Organization.OrganizationMapper as OM
import Shared.Model.Package.Package

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
