module Wizard.Service.KnowledgeModel.Migration.KnowledgeModelMigrationMapper where

import Data.Time
import qualified Data.UUID as U

import Shared.KnowledgeModel.Constant.KnowledgeModel
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Wizard.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationDTO
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Wizard.Model.KnowledgeModel.Migration.KnowledgeModelMigration
import Wizard.Model.KnowledgeModel.Package.KnowledgeModelPackageSuggestion

toDTO :: KnowledgeModelMigration -> KnowledgeModelPackageSuggestion -> KnowledgeModelPackageSuggestion -> KnowledgeModelEditor -> KnowledgeModelMigrationDTO
toDTO ms previousPackage targetPackage kmEditor =
  KnowledgeModelMigrationDTO
    { editorUuid = ms.editorUuid
    , editorName = kmEditor.name
    , editorPreviousPackage = previousPackage
    , state = ms.state
    , targetPackage = targetPackage
    , currentKnowledgeModel = ms.currentKnowledgeModel
    }

fromCreateDTO :: KnowledgeModelEditor -> KnowledgeModelPackage -> [KnowledgeModelEvent] -> U.UUID -> [KnowledgeModelEvent] -> KnowledgeModel -> U.UUID -> UTCTime -> KnowledgeModelMigration
fromCreateDTO kmEditor previousPkg editorPreviousPackageEvents targetPkgUuid targetPkgEvents km tenantUuid now =
  KnowledgeModelMigration
    { editorUuid = kmEditor.uuid
    , metamodelVersion = knowledgeModelMetamodelVersion
    , state = RunningKnowledgeModelMigrationState
    , editorPreviousPackageUuid = previousPkg.uuid
    , targetPackageUuid = targetPkgUuid
    , editorPreviousPackageEvents = editorPreviousPackageEvents
    , targetPackageEvents = targetPkgEvents
    , resultEvents = []
    , currentKnowledgeModel = Just km
    , tenantUuid = tenantUuid
    , createdAt = now
    }
