module Wizard.Specs.Service.Migration.KnowledgeModel.Migrator.Common where

import Data.Maybe
import qualified Data.UUID as U

import Shared.Constant.KnowledgeModel
import Shared.Model.Event.Event
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Util.Date
import Wizard.Database.Migration.Development.App.Data.Apps
import Wizard.Model.App.App
import Wizard.Model.Migration.KnowledgeModel.MigratorState

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
    , appUuid = defaultApp.uuid
    , createdAt = dt'' 2018 1 1 1
    }
