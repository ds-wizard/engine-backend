module Model.Migrator.MigratorState where

import Control.Lens ((^.), makeLenses, (&), (.~))
import Data.Tree
import qualified Data.UUID as U
import GHC.Generics

import Common.Error
import Model.Event.Event
import Model.KnowledgeModel.KnowledgeModel

data MigrationState
  = RunningState
  | ConflictState Conflict
  | ErrorState AppError
  | CompletedState
  deriving (Show, Eq, Generic)

data Conflict =
  CorrectorConflict Event
  deriving (Show, Eq, Generic)

data MigrationConflictAction
  = MCAApply
  | MCAEdited
  | MCAReject
  deriving (Show, Eq, Generic)

data MigratorState = MigratorState
  { _msBranchUuid :: U.UUID
  , _msMigrationState :: MigrationState
  , _msBranchParentId :: String
  , _msTargetPackageId :: String
  , _msBranchEvents :: [Event]
  , _msTargetPackageEvents :: [Event]
  , _msResultEvents :: [Event]
  , _msCurrentKnowledgeModel :: Maybe KnowledgeModel
  } deriving (Show, Eq)

makeLenses ''MigratorState
