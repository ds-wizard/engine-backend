module Service.Migration.KnowledgeModel.MigratorMapper where

import Control.Lens ((^.))

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
  , _migratorStateDTOMigrationState = ms ^. migrationState
  , _migratorStateDTOBranchPreviousPackageId = ms ^. branchPreviousPackageId
  , _migratorStateDTOTargetPackageId = ms ^. targetPackageId
  , _migratorStateDTOCurrentKnowledgeModel = toKnowledgeModelDTO <$> ms ^. currentKnowledgeModel
  }

fromDetailDTO :: MigratorStateDetailDTO -> MigratorState
fromDetailDTO dto =
  MigratorState
  { _migratorStateBranchUuid = dto ^. branchUuid
  , _migratorStateMetamodelVersion = dto ^. metamodelVersion
  , _migratorStateMigrationState = dto ^. migrationState
  , _migratorStateBranchPreviousPackageId = dto ^. branchPreviousPackageId
  , _migratorStateTargetPackageId = dto ^. targetPackageId
  , _migratorStateBranchEvents = fromDTOs (dto ^. branchEvents)
  , _migratorStateTargetPackageEvents = fromDTOs (dto ^. targetPackageEvents)
  , _migratorStateResultEvents = fromDTOs (dto ^. resultEvents)
  , _migratorStateCurrentKnowledgeModel = fromKnowledgeModelDTO <$> dto ^. currentKnowledgeModel
  }

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
