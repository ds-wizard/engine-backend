module Wizard.Model.KnowledgeModel.Migration.KnowledgeModelMigration where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel

data KnowledgeModelMigrationState
  = RunningKnowledgeModelMigrationState
  | ConflictKnowledgeModelMigrationState {targetEvent :: Maybe KnowledgeModelEvent}
  | ErrorKnowledgeModelMigrationState
  | CompletedKnowledgeModelMigrationState
  deriving (Show, Eq, Generic)

data KnowledgeModelMigrationAction
  = ApplyKnowledgeModelMigrationAction
  | RejectKnowledgeModelMigrationAction
  deriving (Show, Eq, Generic)

data KnowledgeModelMigration = KnowledgeModelMigration
  { editorUuid :: U.UUID
  , metamodelVersion :: Int
  , state :: KnowledgeModelMigrationState
  , editorPreviousPackageId :: String
  , targetPackageId :: String
  , editorPreviousPackageEvents :: [KnowledgeModelEvent]
  , targetPackageEvents :: [KnowledgeModelEvent]
  , resultEvents :: [KnowledgeModelEvent]
  , currentKnowledgeModel :: Maybe KnowledgeModel
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Eq)
