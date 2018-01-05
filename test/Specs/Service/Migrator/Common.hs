module Specs.Service.Migrator.Common where

import Data.Maybe
import qualified Data.UUID as U

import Model.Event.Event
import Model.KnowledgeModel.KnowledgeModel
import Model.Migrator.MigratorState

createTestMigratorStateWithEvents :: [Event] -> [Event] -> Maybe KnowledgeModel -> MigratorState
createTestMigratorStateWithEvents branchEvents targetPackageEvents mKm =
  MigratorState
  { _msBranchUuid = fromJust . U.fromString $ "09080ce7-f513-4493-9583-dce567b8e9c5"
  , _msMigrationState = RunningState
  , _msBranchParentId = "b"
  , _msTargetPackageId = "t"
  , _msBranchEvents = branchEvents
  , _msTargetPackageEvents = targetPackageEvents
  , _msResultEvents = []
  , _msCurrentKnowledgeModel = mKm
  }
