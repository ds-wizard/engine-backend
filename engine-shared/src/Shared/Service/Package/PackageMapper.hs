module Shared.Service.Package.PackageMapper where

import qualified Data.UUID as U

import Shared.Api.Resource.Package.PackageDTO
import Shared.Model.Package.Package
import Shared.Model.Package.PackageSimple
import Shared.Model.Package.PackageWithEvents

toPackage :: PackageWithEvents -> Package
toPackage pkg =
  Package
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
    , appUuid = pkg.appUuid
    , createdAt = pkg.createdAt
    }

toSimple :: Package -> PackageSimple
toSimple pkg =
  PackageSimple
    { pId = pkg.pId
    , name = pkg.name
    , version = pkg.version
    }

toDTO :: PackageWithEvents -> PackageDTO
toDTO pkg =
  PackageDTO
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

fromDTO :: PackageDTO -> U.UUID -> PackageWithEvents
fromDTO dto appUuid =
  PackageWithEvents
    { pId = dto.pId
    , name = dto.name
    , organizationId = dto.organizationId
    , kmId = dto.kmId
    , version = dto.version
    , phase = dto.phase
    , metamodelVersion = dto.metamodelVersion
    , description = dto.description
    , readme = dto.readme
    , license = dto.license
    , previousPackageId = dto.previousPackageId
    , forkOfPackageId = dto.forkOfPackageId
    , mergeCheckpointPackageId = dto.mergeCheckpointPackageId
    , events = dto.events
    , appUuid = appUuid
    , createdAt = dto.createdAt
    }
