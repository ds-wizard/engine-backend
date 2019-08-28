module Service.Migration.KnowledgeModel.MigratorMapper where

import Control.Lens ((^.))

import Api.Resource.Migration.KnowledgeModel.MigrationStateDTO
import Api.Resource.Migration.KnowledgeModel.MigratorStateDTO
import Api.Resource.Migration.KnowledgeModel.MigratorStateDetailDTO
import Constant.KnowledgeModel
import LensesConfig
import Model.Branch.Branch
import Model.Event.Event
import Model.KnowledgeModel.KnowledgeModel
import Model.Migration.KnowledgeModel.MigratorState
import Model.Package.Package
import Service.Event.EventMapper
import Service.KnowledgeModel.KnowledgeModelMapper

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
toMigrationStateDTO (ConflictState conflict) = ConflictStateDTO conflict
toMigrationStateDTO ErrorState = ErrorStateDTO
toMigrationStateDTO CompletedState = CompletedStateDTO

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
fromMigrationStateDTO (ConflictStateDTO conflict) = ConflictState conflict
fromMigrationStateDTO ErrorStateDTO = ErrorState
fromMigrationStateDTO CompletedStateDTO = CompletedState

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
