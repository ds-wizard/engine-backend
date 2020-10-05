module Wizard.Database.BSON.Migration.KnowledgeModel.MigratorState where

import qualified Data.Bson as BSON
import Data.Bson.Generic
import Data.Maybe

import Shared.Database.BSON.Common ()
import Shared.Database.BSON.Event.Common ()
import Shared.Model.KnowledgeModel.KnowledgeModel
import Wizard.Model.Migration.KnowledgeModel.MigratorState

instance ToBSON MigrationState where
  toBSON RunningState = ["stateType" BSON.=: "RunningState"]
  toBSON (ConflictState (CorrectorConflict event)) = ["stateType" BSON.=: "ConflictState", "targetEvent" BSON.=: event]
  toBSON ErrorState = ["stateType" BSON.=: "ErrorState"]
  toBSON CompletedState = ["stateType" BSON.=: "CompletedState"]

instance FromBSON MigrationState where
  fromBSON doc = do
    stateType <- BSON.lookup "stateType" doc
    case stateType of
      "RunningState" -> return RunningState
      "ConflictState" -> do
        event <- BSON.lookup "targetEvent" doc
        return . ConflictState . CorrectorConflict . fromJust $ event
      "ErrorState" -> return ErrorState
      "CompletedState" -> return CompletedState

instance ToBSON MigratorState where
  toBSON MigratorState {..} =
    [ "branchUuid" BSON.=: _migratorStateBranchUuid
    , "metamodelVersion" BSON.=: _migratorStateMetamodelVersion
    , "migrationState" BSON.=: _migratorStateMigrationState
    , "branchPreviousPackageId" BSON.=: _migratorStateBranchPreviousPackageId
    , "targetPackageId" BSON.=: _migratorStateTargetPackageId
    , "branchEvents" BSON.=: _migratorStateBranchEvents
    , "targetPackageEvents" BSON.=: _migratorStateTargetPackageEvents
    , "resultEvents" BSON.=: _migratorStateResultEvents
    , "currentKnowledgeModel" BSON.=: (Nothing :: Maybe KnowledgeModel)
    ]

instance FromBSON MigratorState where
  fromBSON doc = do
    _migratorStateBranchUuid <- BSON.lookup "branchUuid" doc
    _migratorStateMetamodelVersion <- BSON.lookup "metamodelVersion" doc
    _migratorStateMigrationState <- BSON.lookup "migrationState" doc
    _migratorStateBranchPreviousPackageId <- BSON.lookup "branchPreviousPackageId" doc
    _migratorStateTargetPackageId <- BSON.lookup "targetPackageId" doc
    _migratorStateBranchEvents <- BSON.lookup "branchEvents" doc
    _migratorStateTargetPackageEvents <- BSON.lookup "targetPackageEvents" doc
    _migratorStateResultEvents <- BSON.lookup "resultEvents" doc
    let _migratorStateCurrentKnowledgeModel = Nothing
    return MigratorState {..}
