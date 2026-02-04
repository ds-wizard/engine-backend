module Wizard.Database.Migration.Development.KnowledgeModel.Data.Migration.KnowledgeModelMigrations where

import Shared.Common.Util.Date
import Shared.KnowledgeModel.Constant.KnowledgeModel
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Event.KnowledgeModelEvents
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.KnowledgeModel.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper
import Wizard.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationCreateDTO
import Wizard.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationDTO
import Wizard.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationResolutionDTO
import Wizard.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Wizard.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Wizard.Database.Migration.Development.Tenant.Data.Tenants
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorList
import Wizard.Model.KnowledgeModel.Migration.KnowledgeModelMigration
import Wizard.Model.Tenant.Tenant
import qualified Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper as PM

knowledgeModelMigrationDTO :: KnowledgeModelMigrationDTO
knowledgeModelMigrationDTO =
  KnowledgeModelMigrationDTO
    { editorUuid = amsterdamKnowledgeModelEditorList.uuid
    , editorName = amsterdamKnowledgeModelEditorList.name
    , editorPreviousPackage = PM.toSuggestion netherlandsKmPackage
    , state = ConflictKnowledgeModelMigrationState {targetEvent = Just . Prelude.head . fmap toEvent $ netherlandsKmPackageV2Events}
    , targetPackage = PM.toSuggestion netherlandsKmPackageV2
    , currentKnowledgeModel = Just km1Netherlands
    }

knowledgeModelMigrationCreateDTO :: KnowledgeModelMigrationCreateDTO
knowledgeModelMigrationCreateDTO = KnowledgeModelMigrationCreateDTO {targetPackageUuid = netherlandsKmPackageV2.uuid}

knowledgeModelMigrationResolutionDTO :: KnowledgeModelMigrationResolutionDTO
knowledgeModelMigrationResolutionDTO =
  KnowledgeModelMigrationResolutionDTO
    { originalEventUuid = a_km1_ch4.uuid
    , action = RejectKnowledgeModelMigrationAction
    }

differentKnowledgeModelMigration :: KnowledgeModelMigration
differentKnowledgeModelMigration =
  KnowledgeModelMigration
    { editorUuid = differentKnowledgeModelEditor.uuid
    , metamodelVersion = knowledgeModelMetamodelVersion
    , state = CompletedKnowledgeModelMigrationState
    , editorPreviousPackageUuid = differentPackage.uuid
    , targetPackageUuid = differentPackage.uuid
    , editorPreviousPackageEvents = []
    , targetPackageEvents = []
    , resultEvents = []
    , currentKnowledgeModel = Nothing
    , tenantUuid = differentTenant.uuid
    , createdAt = dt'' 2018 1 1 1
    }
