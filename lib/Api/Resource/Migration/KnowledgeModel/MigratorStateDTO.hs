module Api.Resource.Migration.KnowledgeModel.MigratorStateDTO where

import qualified Data.UUID as U
import GHC.Generics

import Api.Resource.KnowledgeModel.KnowledgeModelDTO
import Model.Migration.KnowledgeModel.MigratorState

data MigratorStateDTO = MigratorStateDTO
  { _migratorStateDTOBranchUuid :: U.UUID
  , _migratorStateDTOMigrationState :: MigrationState
  , _migratorStateDTOBranchPreviousPackageId :: String
  , _migratorStateDTOTargetPackageId :: String
  , _migratorStateDTOCurrentKnowledgeModel :: Maybe KnowledgeModelDTO
  } deriving (Show, Eq, Generic)
