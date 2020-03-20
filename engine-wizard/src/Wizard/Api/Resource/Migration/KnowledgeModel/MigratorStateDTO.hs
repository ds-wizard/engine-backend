module Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateDTO where

import qualified Data.UUID as U
import GHC.Generics

import Shared.Api.Resource.KnowledgeModel.KnowledgeModelDTO
import Wizard.Api.Resource.Migration.KnowledgeModel.MigrationStateDTO

data MigratorStateDTO =
  MigratorStateDTO
    { _migratorStateDTOBranchUuid :: U.UUID
    , _migratorStateDTOMigrationState :: MigrationStateDTO
    , _migratorStateDTOBranchPreviousPackageId :: String
    , _migratorStateDTOTargetPackageId :: String
    , _migratorStateDTOCurrentKnowledgeModel :: Maybe KnowledgeModelDTO
    }
  deriving (Show, Eq, Generic)
