module Wizard.Specs.Service.Migration.KnowledgeModel.Migrator.Common where

import Data.Maybe
import qualified Data.UUID as U

import Shared.Common.Util.Date
import Wizard.Database.Migration.Development.Tenant.Data.Tenants
import Wizard.Model.Migration.KnowledgeModel.MigratorState
import Wizard.Model.Tenant.Tenant
import WizardLib.KnowledgeModel.Constant.KnowledgeModel
import WizardLib.KnowledgeModel.Model.Event.Event
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel

createTestMigratorStateWithEvents :: [Event] -> [Event] -> Maybe KnowledgeModel -> MigratorState
createTestMigratorStateWithEvents branchEvents targetPackageEvents mKm =
  MigratorState
    { branchUuid = fromJust . U.fromString $ "09080ce7-f513-4493-9583-dce567b8e9c5"
    , metamodelVersion = kmMetamodelVersion
    , migrationState = RunningState
    , branchPreviousPackageId = "b"
    , targetPackageId = "t"
    , branchEvents = branchEvents
    , targetPackageEvents = targetPackageEvents
    , resultEvents = []
    , currentKnowledgeModel = mKm
    , tenantUuid = defaultTenant.uuid
    , createdAt = dt'' 2018 1 1 1
    }
