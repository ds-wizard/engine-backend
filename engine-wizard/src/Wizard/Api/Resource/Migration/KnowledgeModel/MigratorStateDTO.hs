module Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateDTO where

import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.KnowledgeModel.KnowledgeModel
import Wizard.Model.Migration.KnowledgeModel.MigratorState

data MigratorStateDTO =
  MigratorStateDTO
    { _migratorStateDTOBranchUuid :: U.UUID
    , _migratorStateDTOBranchName :: String
    , _migratorStateDTOBranchPreviousPackageId :: String
    , _migratorStateDTOMigrationState :: MigrationState
    , _migratorStateDTOTargetPackageId :: String
    , _migratorStateDTOCurrentKnowledgeModel :: Maybe KnowledgeModel
    }
  deriving (Show, Eq, Generic)
