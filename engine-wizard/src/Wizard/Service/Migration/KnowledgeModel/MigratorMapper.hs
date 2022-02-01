module Wizard.Service.Migration.KnowledgeModel.MigratorMapper where

import Control.Lens ((^.))
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Shared.Constant.KnowledgeModel
import Shared.Model.Event.Event
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.Package.Package
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateDTO
import Wizard.Model.Branch.Branch
import Wizard.Model.Migration.KnowledgeModel.MigratorState

toDTO :: MigratorState -> Branch -> MigratorStateDTO
toDTO ms branch =
  MigratorStateDTO
    { _migratorStateDTOBranchUuid = ms ^. branchUuid
    , _migratorStateDTOBranchName = branch ^. name
    , _migratorStateDTOBranchPreviousPackageId = ms ^. branchPreviousPackageId
    , _migratorStateDTOMigrationState = ms ^. migrationState
    , _migratorStateDTOTargetPackageId = ms ^. targetPackageId
    , _migratorStateDTOCurrentKnowledgeModel = ms ^. currentKnowledgeModel
    }

fromCreateDTO ::
     Branch -> Package -> [Event] -> String -> [Event] -> KnowledgeModel -> U.UUID -> UTCTime -> MigratorState
fromCreateDTO branch previousPkg branchEvents targetPkgId targetPkgEvents km appUuid now =
  MigratorState
    { _migratorStateBranchUuid = branch ^. uuid
    , _migratorStateMetamodelVersion = kmMetamodelVersion
    , _migratorStateMigrationState = RunningState
    , _migratorStateBranchPreviousPackageId = previousPkg ^. pId
    , _migratorStateTargetPackageId = targetPkgId
    , _migratorStateBranchEvents = branchEvents
    , _migratorStateTargetPackageEvents = targetPkgEvents
    , _migratorStateResultEvents = []
    , _migratorStateCurrentKnowledgeModel = Just km
    , _migratorStateAppUuid = appUuid
    , _migratorStateCreatedAt = now
    }
