module Wizard.Specs.Service.KnowledgeModel.Migration.Migrator.Common where

import Data.Maybe
import qualified Data.UUID as U

import Shared.Common.Util.Date
import Shared.KnowledgeModel.Constant.KnowledgeModel
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Wizard.Database.Migration.Development.Tenant.Data.Tenants
import Wizard.Model.KnowledgeModel.Migration.KnowledgeModelMigration
import Wizard.Model.Tenant.Tenant

createTestMigratorStateWithEvents :: [KnowledgeModelEvent] -> [KnowledgeModelEvent] -> Maybe KnowledgeModel -> KnowledgeModelMigration
createTestMigratorStateWithEvents editorEvents targetPackageEvents mKm =
  KnowledgeModelMigration
    { editorUuid = fromJust . U.fromString $ "09080ce7-f513-4493-9583-dce567b8e9c5"
    , metamodelVersion = kmMetamodelVersion
    , state = RunningKnowledgeModelMigrationState
    , editorPreviousPackageId = "b"
    , targetPackageId = "t"
    , editorPreviousPackageEvents = editorEvents
    , targetPackageEvents = targetPackageEvents
    , resultEvents = []
    , currentKnowledgeModel = mKm
    , tenantUuid = defaultTenant.uuid
    , createdAt = dt'' 2018 1 1 1
    }
