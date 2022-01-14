module Wizard.Specs.Service.Migration.KnowledgeModel.Migrator.Common where

import Control.Lens ((^.))
import Data.Maybe
import qualified Data.UUID as U

import LensesConfig
import Shared.Constant.KnowledgeModel
import Shared.Model.Event.Event
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Util.Date
import Wizard.Database.Migration.Development.App.Data.Apps
import Wizard.Model.Migration.KnowledgeModel.MigratorState

createTestMigratorStateWithEvents :: [Event] -> [Event] -> Maybe KnowledgeModel -> MigratorState
createTestMigratorStateWithEvents branchEvents targetPackageEvents mKm =
  MigratorState
    { _migratorStateBranchUuid = fromJust . U.fromString $ "09080ce7-f513-4493-9583-dce567b8e9c5"
    , _migratorStateMetamodelVersion = kmMetamodelVersion
    , _migratorStateMigrationState = RunningState
    , _migratorStateBranchPreviousPackageId = "b"
    , _migratorStateTargetPackageId = "t"
    , _migratorStateBranchEvents = branchEvents
    , _migratorStateTargetPackageEvents = targetPackageEvents
    , _migratorStateResultEvents = []
    , _migratorStateCurrentKnowledgeModel = mKm
    , _migratorStateAppUuid = defaultApp ^. uuid
    , _migratorStateCreatedAt = dt'' 2018 1 1 1
    }
