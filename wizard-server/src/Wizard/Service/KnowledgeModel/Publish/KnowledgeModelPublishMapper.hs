module Wizard.Service.KnowledgeModel.Publish.KnowledgeModelPublishMapper where

import Data.Time
import qualified Data.UUID as U

import Shared.Coordinate.Model.Coordinate.Coordinate
import Shared.KnowledgeModel.Constant.KnowledgeModel
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackageEvent
import Shared.KnowledgeModel.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Wizard.Model.Tenant.Config.TenantConfig

fromPackage
  :: KnowledgeModelEditor
  -> U.UUID
  -> Maybe Coordinate
  -> Maybe Coordinate
  -> TenantConfigOrganization
  -> String
  -> String
  -> String
  -> [KnowledgeModelEvent]
  -> UTCTime
  -> (KnowledgeModelPackage, [KnowledgeModelPackageEvent])
fromPackage editor uuid forkOfPkgId mergeCheckpointPkgId org version description readme events now =
  ( KnowledgeModelPackage
      { uuid = uuid
      , name = editor.name
      , organizationId = org.organizationId
      , kmId = editor.kmId
      , version = version
      , phase = ReleasedKnowledgeModelPackagePhase
      , metamodelVersion = knowledgeModelMetamodelVersion
      , description = description
      , readme = readme
      , license = editor.license
      , previousPackageUuid = editor.previousPackageUuid
      , forkOfPackageId = forkOfPkgId
      , mergeCheckpointPackageId = mergeCheckpointPkgId
      , nonEditable = False
      , public = False
      , tenantUuid = editor.tenantUuid
      , createdAt = now
      }
  , fmap (toPackageEvent uuid editor.tenantUuid) events
  )
