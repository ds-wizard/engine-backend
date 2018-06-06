module Service.Migrator.MigratorMapper where

import Control.Lens ((^.))

import Api.Resource.Migrator.MigratorStateDTO
import LensesConfig
import Model.Migrator.MigratorState
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
