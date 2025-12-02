module Shared.KnowledgeModel.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper where

import qualified Data.UUID as U

import Shared.KnowledgeModel.Model.KnowledgeModel.Bundle.KnowledgeModelBundlePackage
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelRawEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackageEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackageRawEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackageSimple

toPackageEvent :: String -> U.UUID -> KnowledgeModelEvent -> KnowledgeModelPackageEvent
toPackageEvent packageId tenantUuid KnowledgeModelEvent {..} = KnowledgeModelPackageEvent {..}

toPackageRawEvent :: String -> U.UUID -> KnowledgeModelRawEvent -> KnowledgeModelPackageRawEvent
toPackageRawEvent packageId tenantUuid KnowledgeModelRawEvent {..} = KnowledgeModelPackageRawEvent {..}

toEvent :: KnowledgeModelPackageEvent -> KnowledgeModelEvent
toEvent KnowledgeModelPackageEvent {..} = KnowledgeModelEvent {..}

toRawEvent :: KnowledgeModelPackageRawEvent -> KnowledgeModelRawEvent
toRawEvent KnowledgeModelPackageRawEvent {..} = KnowledgeModelRawEvent {..}

toSimple :: KnowledgeModelPackage -> KnowledgeModelPackageSimple
toSimple pkg =
  KnowledgeModelPackageSimple
    { pId = pkg.pId
    , name = pkg.name
    , version = pkg.version
    }

toKnowledgeModelBundlePackage :: KnowledgeModelPackage -> [KnowledgeModelPackageEvent] -> KnowledgeModelBundlePackage
toKnowledgeModelBundlePackage pkg pkgEvents =
  KnowledgeModelBundlePackage
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
    , events = fmap toEvent pkgEvents
    , createdAt = pkg.createdAt
    }

fromKnowledgeModelBundlePackage :: KnowledgeModelBundlePackage -> U.UUID -> (KnowledgeModelPackage, [KnowledgeModelPackageEvent])
fromKnowledgeModelBundlePackage dto tenantUuid =
  ( KnowledgeModelPackage
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
      , tenantUuid = tenantUuid
      , createdAt = dto.createdAt
      }
  , fmap (toPackageEvent dto.pId tenantUuid) dto.events
  )
