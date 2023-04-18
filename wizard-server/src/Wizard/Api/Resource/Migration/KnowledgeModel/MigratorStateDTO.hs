module Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateDTO where

import qualified Data.UUID as U
import GHC.Generics

import Wizard.Model.Migration.KnowledgeModel.MigratorState
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel

data MigratorStateDTO = MigratorStateDTO
  { branchUuid :: U.UUID
  , branchName :: String
  , branchPreviousPackageId :: String
  , migrationState :: MigrationState
  , targetPackageId :: String
  , currentKnowledgeModel :: Maybe KnowledgeModel
  }
  deriving (Show, Eq, Generic)
