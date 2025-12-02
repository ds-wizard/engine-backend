module Wizard.Service.KnowledgeModel.Migration.MigrationMapper where

import Data.Time
import qualified Data.UUID as U

import Shared.KnowledgeModel.Constant.KnowledgeModel
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Wizard.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationDTO
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Wizard.Model.KnowledgeModel.Migration.KnowledgeModelMigration

toDTO :: KnowledgeModelMigration -> KnowledgeModelEditor -> KnowledgeModelMigrationDTO
toDTO ms kmEditor =
  KnowledgeModelMigrationDTO
    { editorUuid = ms.editorUuid
    , editorName = kmEditor.name
    , editorPreviousPackageId = ms.editorPreviousPackageId
    , state = ms.state
    , targetPackageId = ms.targetPackageId
    , currentKnowledgeModel = ms.currentKnowledgeModel
    }

fromCreateDTO :: KnowledgeModelEditor -> KnowledgeModelPackage -> [KnowledgeModelEvent] -> String -> [KnowledgeModelEvent] -> KnowledgeModel -> U.UUID -> UTCTime -> KnowledgeModelMigration
fromCreateDTO kmEditor previousPkg editorPreviousPackageEvents targetPkgId targetPkgEvents km tenantUuid now =
  KnowledgeModelMigration
    { editorUuid = kmEditor.uuid
    , metamodelVersion = kmMetamodelVersion
    , state = RunningKnowledgeModelMigrationState
    , editorPreviousPackageId = previousPkg.pId
    , targetPackageId = targetPkgId
    , editorPreviousPackageEvents = editorPreviousPackageEvents
    , targetPackageEvents = targetPkgEvents
    , resultEvents = []
    , currentKnowledgeModel = Just km
    , tenantUuid = tenantUuid
    , createdAt = now
    }
