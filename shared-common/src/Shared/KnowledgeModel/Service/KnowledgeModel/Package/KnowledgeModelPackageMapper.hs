module Shared.KnowledgeModel.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper where

import qualified Data.UUID as U

import Shared.Coordinate.Model.Coordinate.Coordinate
import Shared.KnowledgeModel.Model.KnowledgeModel.Bundle.KnowledgeModelBundlePackage
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelRawEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackageEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackageRawEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackageSimple

toPackageEvent :: U.UUID -> U.UUID -> KnowledgeModelEvent -> KnowledgeModelPackageEvent
toPackageEvent packageUuid tenantUuid KnowledgeModelEvent {..} = KnowledgeModelPackageEvent {..}

toPackageRawEvent :: U.UUID -> U.UUID -> KnowledgeModelRawEvent -> KnowledgeModelPackageRawEvent
toPackageRawEvent packageUuid tenantUuid KnowledgeModelRawEvent {..} = KnowledgeModelPackageRawEvent {..}

toEvent :: KnowledgeModelPackageEvent -> KnowledgeModelEvent
toEvent KnowledgeModelPackageEvent {..} = KnowledgeModelEvent {..}

toRawEvent :: KnowledgeModelPackageRawEvent -> KnowledgeModelRawEvent
toRawEvent KnowledgeModelPackageRawEvent {..} = KnowledgeModelRawEvent {..}

toSimple :: KnowledgeModelPackage -> KnowledgeModelPackageSimple
toSimple pkg =
  KnowledgeModelPackageSimple
    { uuid = pkg.uuid
    , name = pkg.name
    , version = pkg.version
    }

toKnowledgeModelBundlePackage :: KnowledgeModelPackage -> [KnowledgeModelPackageEvent] -> Maybe Coordinate -> KnowledgeModelBundlePackage
toKnowledgeModelBundlePackage pkg pkgEvents previousPackageCoordinate =
  KnowledgeModelBundlePackage
    { pId = createCoordinate pkg
    , name = pkg.name
    , organizationId = pkg.organizationId
    , kmId = pkg.kmId
    , version = pkg.version
    , phase = pkg.phase
    , metamodelVersion = pkg.metamodelVersion
    , description = pkg.description
    , readme = pkg.readme
    , license = pkg.license
    , previousPackageId = previousPackageCoordinate
    , forkOfPackageId = pkg.forkOfPackageId
    , mergeCheckpointPackageId = pkg.mergeCheckpointPackageId
    , nonEditable = pkg.nonEditable
    , events = fmap toEvent pkgEvents
    , createdAt = pkg.createdAt
    }

fromKnowledgeModelBundlePackage :: KnowledgeModelBundlePackage -> U.UUID -> Maybe U.UUID -> U.UUID -> (KnowledgeModelPackage, [KnowledgeModelPackageEvent])
fromKnowledgeModelBundlePackage dto pkgUuid previousPackageUuid tenantUuid =
  ( KnowledgeModelPackage
      { uuid = pkgUuid
      , name = dto.name
      , organizationId = dto.organizationId
      , kmId = dto.kmId
      , version = dto.version
      , phase = dto.phase
      , metamodelVersion = dto.metamodelVersion
      , description = dto.description
      , readme = dto.readme
      , license = dto.license
      , previousPackageUuid = previousPackageUuid
      , forkOfPackageId = dto.forkOfPackageId
      , mergeCheckpointPackageId = dto.mergeCheckpointPackageId
      , nonEditable = dto.nonEditable
      , public = False
      , tenantUuid = tenantUuid
      , createdAt = dto.createdAt
      }
  , fmap (toPackageEvent pkgUuid tenantUuid) dto.events
  )
