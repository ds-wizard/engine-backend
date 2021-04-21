module Registry.Service.Package.PackageMapper where

import Control.Lens ((^.))
import Data.Aeson

import LensesConfig
import Registry.Api.Resource.Package.PackageDetailDTO
import Registry.Api.Resource.Package.PackageRawDTO
import Registry.Api.Resource.Package.PackageSimpleDTO
import Registry.Model.Organization.Organization
import qualified Registry.Service.Organization.OrganizationMapper as OM
import Shared.Api.Resource.Event.EventJM ()
import Shared.Model.Package.Package
import Shared.Model.Package.PackageWithEvents
import Shared.Model.Package.PackageWithEventsRaw

toRaw :: PackageWithEvents -> PackageWithEventsRaw
toRaw pkg =
  PackageWithEventsRaw
    { _packageWithEventsRawPId = pkg ^. pId
    , _packageWithEventsRawName = pkg ^. name
    , _packageWithEventsRawOrganizationId = pkg ^. organizationId
    , _packageWithEventsRawKmId = pkg ^. kmId
    , _packageWithEventsRawVersion = pkg ^. version
    , _packageWithEventsRawMetamodelVersion = pkg ^. metamodelVersion
    , _packageWithEventsRawDescription = pkg ^. description
    , _packageWithEventsRawReadme = pkg ^. readme
    , _packageWithEventsRawLicense = pkg ^. license
    , _packageWithEventsRawPreviousPackageId = pkg ^. previousPackageId
    , _packageWithEventsRawForkOfPackageId = pkg ^. forkOfPackageId
    , _packageWithEventsRawMergeCheckpointPackageId = pkg ^. mergeCheckpointPackageId
    , _packageWithEventsRawEvents = toJSON $ pkg ^. events
    , _packageWithEventsRawCreatedAt = pkg ^. createdAt
    }

toRawDTO :: PackageWithEventsRaw -> PackageRawDTO
toRawDTO pkg =
  PackageRawDTO
    { _packageRawDTOPId = pkg ^. pId
    , _packageRawDTOName = pkg ^. name
    , _packageRawDTOOrganizationId = pkg ^. organizationId
    , _packageRawDTOKmId = pkg ^. kmId
    , _packageRawDTOVersion = pkg ^. version
    , _packageRawDTOMetamodelVersion = pkg ^. metamodelVersion
    , _packageRawDTODescription = pkg ^. description
    , _packageRawDTOReadme = pkg ^. readme
    , _packageRawDTOLicense = pkg ^. license
    , _packageRawDTOPreviousPackageId = pkg ^. previousPackageId
    , _packageRawDTOForkOfPackageId = pkg ^. forkOfPackageId
    , _packageRawDTOMergeCheckpointPackageId = pkg ^. mergeCheckpointPackageId
    , _packageRawDTOEvents = pkg ^. events
    , _packageRawDTOCreatedAt = pkg ^. createdAt
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
