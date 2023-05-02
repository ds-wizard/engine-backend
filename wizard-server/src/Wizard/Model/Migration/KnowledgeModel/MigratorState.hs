module Wizard.Model.Migration.KnowledgeModel.MigratorState where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import WizardLib.KnowledgeModel.Model.Event.Event
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel

data MigrationState
  = RunningState
  | ConflictState Conflict
  | ErrorState
  | CompletedState
  deriving (Show, Eq, Generic)

data Conflict
  = CorrectorConflict (Maybe Event)
  deriving (Show, Eq, Generic)

data MigrationConflictAction
  = MCAApply
  | MCAEdited
  | MCAReject
  deriving (Show, Eq, Generic)

data MigratorState = MigratorState
  { branchUuid :: U.UUID
  , metamodelVersion :: Int
  , migrationState :: MigrationState
  , branchPreviousPackageId :: String
  , targetPackageId :: String
  , branchEvents :: [Event]
  , targetPackageEvents :: [Event]
  , resultEvents :: [Event]
  , currentKnowledgeModel :: Maybe KnowledgeModel
  , appUuid :: U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Eq)
