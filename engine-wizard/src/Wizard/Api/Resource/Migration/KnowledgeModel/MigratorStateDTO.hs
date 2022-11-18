module Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateDTO where

import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.KnowledgeModel.KnowledgeModel
import Wizard.Model.Migration.KnowledgeModel.MigratorState

data MigratorStateDTO = MigratorStateDTO
  { branchUuid :: U.UUID
  , branchName :: String
  , branchPreviousPackageId :: String
  , migrationState :: MigrationState
  , targetPackageId :: String
  , currentKnowledgeModel :: Maybe KnowledgeModel
  }
  deriving (Show, Eq, Generic)
