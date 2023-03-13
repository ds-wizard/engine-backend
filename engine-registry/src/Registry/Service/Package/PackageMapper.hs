module Registry.Service.Package.PackageMapper where

import Data.Aeson

import Registry.Api.Resource.Package.PackageDetailDTO
import Registry.Api.Resource.Package.PackageRawDTO
import Registry.Api.Resource.Package.PackageSimpleDTO
import qualified Registry.Model.Organization.Organization as Organization
import qualified Registry.Service.Organization.OrganizationMapper as OM
import Shared.Api.Resource.Event.EventJM ()
import Shared.Model.Package.Package
import Shared.Model.Package.PackageWithEvents
import Shared.Model.Package.PackageWithEventsRaw

toRaw :: PackageWithEvents -> PackageWithEventsRaw
toRaw pkg =
  PackageWithEventsRaw
    { pId = pkg.pId
    , name = pkg.name
    , organizationId = pkg.organizationId
    , kmId = pkg.kmId
    , version = pkg.version
    , phase = pkg.phase
    , metamodelVersion = pkg.metamodelVersion
    , description = pkg.description
    , readme = pkg.readme
    , license = pkg.license
    , previousPackageId = pkg.previousPackageId
    , forkOfPackageId = pkg.forkOfPackageId
    , mergeCheckpointPackageId = pkg.mergeCheckpointPackageId
    , events = toJSON $ pkg.events
    , appUuid = pkg.appUuid
    , createdAt = pkg.createdAt
    }

toRawDTO :: PackageWithEventsRaw -> PackageRawDTO
toRawDTO pkg =
  PackageRawDTO
    { pId = pkg.pId
    , name = pkg.name
    , organizationId = pkg.organizationId
    , kmId = pkg.kmId
    , version = pkg.version
    , phase = pkg.phase
    , metamodelVersion = pkg.metamodelVersion
    , description = pkg.description
    , readme = pkg.readme
    , license = pkg.license
    , previousPackageId = pkg.previousPackageId
    , forkOfPackageId = pkg.forkOfPackageId
    , mergeCheckpointPackageId = pkg.mergeCheckpointPackageId
    , events = pkg.events
    , createdAt = pkg.createdAt
    }

toSimpleDTO :: Package -> Organization.Organization -> PackageSimpleDTO
toSimpleDTO pkg org =
  PackageSimpleDTO
    { pId = pkg.pId
    , name = pkg.name
    , organizationId = pkg.organizationId
    , kmId = pkg.kmId
    , version = pkg.version
    , description = pkg.description
    , createdAt = pkg.createdAt
    , organization = OM.toSimpleDTO org
    }

toDetailDTO :: Package -> [String] -> Organization.Organization -> PackageDetailDTO
toDetailDTO pkg versions org =
  PackageDetailDTO
    { pId = pkg.pId
    , name = pkg.name
    , organizationId = pkg.organizationId
    , kmId = pkg.kmId
    , version = pkg.version
    , phase = pkg.phase
    , description = pkg.description
    , readme = pkg.readme
    , license = pkg.license
    , metamodelVersion = pkg.metamodelVersion
    , previousPackageId = pkg.previousPackageId
    , forkOfPackageId = pkg.forkOfPackageId
    , mergeCheckpointPackageId = pkg.mergeCheckpointPackageId
    , versions = versions
    , organization = OM.toSimpleDTO org
    , createdAt = pkg.createdAt
    }
