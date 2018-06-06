module Database.BSON.Migrator.MigratorState where

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic
import Data.Maybe

import Database.BSON.Common
import Database.BSON.Event.Answer ()
import Database.BSON.Event.Chapter ()
import Database.BSON.Event.Common
import Database.BSON.Event.Expert ()
import Database.BSON.Event.KnowledgeModel ()
import Database.BSON.Event.Question ()
import Database.BSON.Event.Reference ()
import Database.BSON.KnowledgeModel.KnowledgeModel ()
import LensesConfig
import Model.Migrator.MigratorState

instance ToBSON MigrationState where
  toBSON RunningState = ["stateType" BSON.=: "RunningState"]
  toBSON (ConflictState (CorrectorConflict event)) =
    ["stateType" BSON.=: "ConflictState", "targetEvent" BSON.=: convertEventToBSON event]
  toBSON (ErrorState appError) = ["stateType" BSON.=: "ErrorState", "error" BSON.=: appError]
  toBSON CompletedState = ["stateType" BSON.=: "CompletedState"]

instance FromBSON MigrationState where
  fromBSON doc = do
    stateType <- BSON.lookup "stateType" doc
    case stateType of
      "RunningState" -> return RunningState
      "ConflictState" -> do
        event <- BSON.lookup "targetEvent" doc
        return . ConflictState . CorrectorConflict . fromJust . chooseEventDeserializator $ event
      "ErrorState" -> do
        error <- BSON.lookup "error" doc
        return . ErrorState $ error
      "CompletedState" -> return CompletedState

instance ToBSON MigratorState where
  toBSON ms =
    [ "branchUuid" BSON.=: serializeUUID (ms ^. branchUuid)
    , "migrationState" BSON.=: (ms ^. migrationState)
    , "branchParentId" BSON.=: (ms ^. branchParentId)
    , "targetPackageId" BSON.=: (ms ^. targetPackageId)
    , "branchEvents" BSON.=: convertEventToBSON <$> (ms ^. branchEvents)
    , "targetPackageEvents" BSON.=: convertEventToBSON <$> (ms ^. targetPackageEvents)
    , "resultEvents" BSON.=: convertEventToBSON <$> (ms ^. resultEvents)
    , "currentKnowledgeModel" BSON.=: (ms ^. currentKnowledgeModel)
    ]

instance FromBSON MigratorState where
  fromBSON doc = do
    msBranchUuid <- deserializeMaybeUUID $ BSON.lookup "branchUuid" doc
    msMigrationState <- BSON.lookup "migrationState" doc
    msBranchParentId <- BSON.lookup "branchParentId" doc
    msTargetPackageId <- BSON.lookup "targetPackageId" doc
    msBranchEventsSerialized <- BSON.lookup "branchEvents" doc
    let msBranchEvents = fmap (fromJust . chooseEventDeserializator) msBranchEventsSerialized
    msTargetPackageEventsSerialized <- BSON.lookup "targetPackageEvents" doc
    let msTargetPackageEvents = fmap (fromJust . chooseEventDeserializator) msTargetPackageEventsSerialized
    msResultEventsSerialized <- BSON.lookup "resultEvents" doc
    let msResultEvents = fmap (fromJust . chooseEventDeserializator) msResultEventsSerialized
    msCurrentKnowledgeModel <- BSON.lookup "currentKnowledgeModel" doc
    return
      MigratorState
      { _migratorStateBranchUuid = msBranchUuid
      , _migratorStateMigrationState = msMigrationState
      , _migratorStateBranchParentId = msBranchParentId
      , _migratorStateTargetPackageId = msTargetPackageId
      , _migratorStateBranchEvents = msBranchEvents
      , _migratorStateTargetPackageEvents = msTargetPackageEvents
      , _migratorStateResultEvents = msResultEvents
      , _migratorStateCurrentKnowledgeModel = msCurrentKnowledgeModel
      }
