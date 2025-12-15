module Wizard.Service.KnowledgeModel.Publish.KnowledgeModelPublishMapper where

import Data.Time

import Shared.Coordinate.Util.Coordinate
import Shared.KnowledgeModel.Constant.KnowledgeModel
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackageEvent
import Shared.KnowledgeModel.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Wizard.Model.Tenant.Config.TenantConfig

fromPackage
  :: KnowledgeModelEditor
  -> Maybe String
  -> Maybe String
  -> TenantConfigOrganization
  -> String
  -> String
  -> String
  -> [KnowledgeModelEvent]
  -> UTCTime
  -> (KnowledgeModelPackage, [KnowledgeModelPackageEvent])
fromPackage editor forkOfPkgId mergeCheckpointPkgId org version description readme events now =
  let pId = buildCoordinate org.organizationId editor.kmId version
   in ( KnowledgeModelPackage
          { pId = pId
          , name = editor.name
          , organizationId = org.organizationId
          , kmId = editor.kmId
          , version = version
          , phase = ReleasedKnowledgeModelPackagePhase
          , metamodelVersion = knowledgeModelMetamodelVersion
          , description = description
          , readme = readme
          , license = editor.license
          , previousPackageId = editor.previousPackageId
          , forkOfPackageId = forkOfPkgId
          , mergeCheckpointPackageId = mergeCheckpointPkgId
          , nonEditable = False
          , tenantUuid = editor.tenantUuid
          , createdAt = now
          }
      , fmap (toPackageEvent pId editor.tenantUuid) events
      )
