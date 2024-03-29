module WizardLib.KnowledgeModel.Service.Package.PackageMapper where

import qualified Data.UUID as U

import WizardLib.KnowledgeModel.Api.Resource.Package.PackageDTO
import WizardLib.KnowledgeModel.Model.Package.Package
import WizardLib.KnowledgeModel.Model.Package.PackageSimple
import WizardLib.KnowledgeModel.Model.Package.PackageWithEvents

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
    , nonEditable = pkg.nonEditable
    , tenantUuid = pkg.tenantUuid
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
    , nonEditable = pkg.nonEditable
    , events = pkg.events
    , createdAt = pkg.createdAt
    }

fromDTO :: PackageDTO -> U.UUID -> PackageWithEvents
fromDTO dto tenantUuid =
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
    , nonEditable = dto.nonEditable
    , events = dto.events
    , tenantUuid = tenantUuid
    , createdAt = dto.createdAt
    }
