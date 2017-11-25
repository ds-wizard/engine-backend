module Model.Migrator.MigratorState where

import Control.Lens ((^.), makeLenses, (&), (.~))
import Data.Tree
import qualified Data.UUID as U

import Model.Event.Event
import Model.KnowledgeModel.KnowledgeModel

data MigrationState
  = RunningState
  | ConflictState Conflict
  | ErrorState MigrationError
  | CompletedState
  deriving (Show, Eq)

data MigrationError =
  ApplicatorError String
  deriving (Show, Eq)

data Conflict =
  CorrectorConflict
  deriving (Show, Eq)

data MigratorState = MigratorState
  { _msMigrationState :: MigrationState
  , _msBranchParentId :: String
  , _msTargetPackageId :: String
  , _msBranchEvents :: [Event]
  , _msTargetPackageEvents :: [Event]
  , _msCurrentKnowledgeModel :: Maybe KnowledgeModel
  } deriving (Show, Eq)

makeLenses ''MigratorState

convertToErrorState :: MigratorState -> MigrationError -> MigratorState
convertToErrorState state error = state & msMigrationState .~ ErrorState error
