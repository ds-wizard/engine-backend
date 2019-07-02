module Service.Migration.KnowledgeModel.MigratorMapper where

import Control.Lens ((^.))

import Api.Resource.Migration.KnowledgeModel.MigratorStateDTO
import Api.Resource.Migration.KnowledgeModel.MigratorStateDetailDTO
import LensesConfig
import Model.Migration.KnowledgeModel.MigratorState
import Service.Event.EventMapper
import Service.KnowledgeModel.KnowledgeModelMapper

toDTO :: MigratorState -> MigratorStateDTO
toDTO ms =
  MigratorStateDTO
  { _migratorStateDTOBranchUuid = ms ^. branchUuid
  , _migratorStateDTOMigrationState = ms ^. migrationState
  , _migratorStateDTOBranchParentId = ms ^. branchParentId
  , _migratorStateDTOTargetPackageId = ms ^. targetPackageId
  , _migratorStateDTOCurrentKnowledgeModel = toKnowledgeModelDTO <$> ms ^. currentKnowledgeModel
  }

fromDetailDTO :: MigratorStateDetailDTO -> MigratorState
fromDetailDTO dto =
  MigratorState
  { _migratorStateBranchUuid = dto ^. branchUuid
  , _migratorStateMetamodelVersion = dto ^. metamodelVersion
  , _migratorStateMigrationState = dto ^. migrationState
  , _migratorStateBranchParentId = dto ^. branchParentId
  , _migratorStateTargetPackageId = dto ^. targetPackageId
  , _migratorStateBranchEvents = fromDTOs (dto ^. branchEvents)
  , _migratorStateTargetPackageEvents = fromDTOs (dto ^. targetPackageEvents)
  , _migratorStateResultEvents = fromDTOs (dto ^. resultEvents)
  , _migratorStateCurrentKnowledgeModel = fromKnowledgeModelDTO <$> dto ^. currentKnowledgeModel
  }
