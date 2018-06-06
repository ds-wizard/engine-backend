module Api.Resource.Migrator.MigratorStateDTO where

import qualified Data.UUID as U

import Api.Resource.KnowledgeModel.KnowledgeModelDTO
import Model.Migrator.MigratorState

data MigratorStateDTO = MigratorStateDTO
  { _migratorStateDTOBranchUuid :: U.UUID
  , _migratorStateDTOMigrationState :: MigrationState
  , _migratorStateDTOBranchParentId :: String
  , _migratorStateDTOTargetPackageId :: String
  , _migratorStateDTOCurrentKnowledgeModel :: Maybe KnowledgeModelDTO
  } deriving (Show, Eq)
