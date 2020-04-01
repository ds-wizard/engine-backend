module Wizard.Service.Migration.KnowledgeModel.MigratorMapper where

import Control.Lens ((^.))

import LensesConfig
import Shared.Constant.KnowledgeModel
import Shared.Model.Event.Event
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.Package.Package
import Shared.Service.Event.EventMapper
import Wizard.Api.Resource.Migration.KnowledgeModel.MigrationStateDTO
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateDTO
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateDetailDTO
import Wizard.Model.Branch.Branch
import Wizard.Model.Migration.KnowledgeModel.MigratorState
import Wizard.Service.KnowledgeModel.KnowledgeModelMapper

toDTO :: MigratorState -> MigratorStateDTO
toDTO ms =
  MigratorStateDTO
    { _migratorStateDTOBranchUuid = ms ^. branchUuid
    , _migratorStateDTOMigrationState = toMigrationStateDTO $ ms ^. migrationState
    , _migratorStateDTOBranchPreviousPackageId = ms ^. branchPreviousPackageId
    , _migratorStateDTOTargetPackageId = ms ^. targetPackageId
    , _migratorStateDTOCurrentKnowledgeModel = toKnowledgeModelDTO <$> ms ^. currentKnowledgeModel
    }

toMigrationStateDTO :: MigrationState -> MigrationStateDTO
toMigrationStateDTO RunningState = RunningStateDTO
toMigrationStateDTO (ConflictState conflict) = ConflictStateDTO (toConflictDTO conflict)
toMigrationStateDTO ErrorState = ErrorStateDTO
toMigrationStateDTO CompletedState = CompletedStateDTO

toConflictDTO :: Conflict -> ConflictDTO
toConflictDTO (CorrectorConflict event) = CorrectorConflictDTO (toDTOFn event)

fromDetailDTO :: MigratorStateDetailDTO -> MigratorState
fromDetailDTO dto =
  MigratorState
    { _migratorStateBranchUuid = dto ^. branchUuid
    , _migratorStateMetamodelVersion = dto ^. metamodelVersion
    , _migratorStateMigrationState = fromMigrationStateDTO $ dto ^. migrationState
    , _migratorStateBranchPreviousPackageId = dto ^. branchPreviousPackageId
    , _migratorStateTargetPackageId = dto ^. targetPackageId
    , _migratorStateBranchEvents = fromDTOs (dto ^. branchEvents)
    , _migratorStateTargetPackageEvents = fromDTOs (dto ^. targetPackageEvents)
    , _migratorStateResultEvents = fromDTOs (dto ^. resultEvents)
    , _migratorStateCurrentKnowledgeModel = fromKnowledgeModelDTO <$> dto ^. currentKnowledgeModel
    }

fromMigrationStateDTO :: MigrationStateDTO -> MigrationState
fromMigrationStateDTO RunningStateDTO = RunningState
fromMigrationStateDTO (ConflictStateDTO conflict) = ConflictState (fromConflictDTO conflict)
fromMigrationStateDTO ErrorStateDTO = ErrorState
fromMigrationStateDTO CompletedStateDTO = CompletedState

fromConflictDTO :: ConflictDTO -> Conflict
fromConflictDTO (CorrectorConflictDTO event) = CorrectorConflict (fromDTOFn event)

fromCreateDTO :: BranchWithEvents -> Package -> [Event] -> String -> [Event] -> KnowledgeModel -> MigratorState
fromCreateDTO branch previousPkg branchEvents targetPkgId targetPkgEvents km =
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
    }
