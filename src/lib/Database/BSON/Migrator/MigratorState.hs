module Database.BSON.Migrator.MigratorState where

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic
import Data.Maybe
import GHC.Generics

import Database.BSON.Common
import Database.BSON.Event.Answer
import Database.BSON.Event.Chapter
import Database.BSON.Event.Common
import Database.BSON.Event.Expert
import Database.BSON.Event.FollowUpQuestion
import Database.BSON.Event.KnowledgeModel
import Database.BSON.Event.Question
import Database.BSON.Event.Reference
import Database.BSON.KnowledgeModel.KnowledgeModel
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
    [ "branchUuid" BSON.=: serializeUUID (ms ^. msBranchUuid)
    , "migrationState" BSON.=: (ms ^. msMigrationState)
    , "branchParentId" BSON.=: (ms ^. msBranchParentId)
    , "targetPackageId" BSON.=: (ms ^. msTargetPackageId)
    , "branchEvents" BSON.=: convertEventToBSON <$> (ms ^. msBranchEvents)
    , "targetPackageEvents" BSON.=: convertEventToBSON <$> (ms ^. msTargetPackageEvents)
    , "currentKnowledgeModel" BSON.=: (ms ^. msCurrentKnowledgeModel)
    ]

instance FromBSON MigratorState where
  fromBSON doc = do
    branchUuid <- deserializeUUID $ BSON.lookup "branchUuid" doc
    migrationState <- BSON.lookup "migrationState" doc
    branchParentId <- BSON.lookup "branchParentId" doc
    targetPackageId <- BSON.lookup "targetPackageId" doc
    branchEventsSerialized <- BSON.lookup "branchEvents" doc
    let branchEvents = fmap (fromJust . chooseEventDeserializator) branchEventsSerialized
    targetPackageEventsSerialized <- BSON.lookup "targetPackageEvents" doc
    let targetPackageEvents = fmap (fromJust . chooseEventDeserializator) targetPackageEventsSerialized
    currentKnowledgeModel <- BSON.lookup "currentKnowledgeModel" doc
    return
      MigratorState
      { _msBranchUuid = branchUuid
      , _msMigrationState = migrationState
      , _msBranchParentId = branchParentId
      , _msTargetPackageId = targetPackageId
      , _msBranchEvents = branchEvents
      , _msTargetPackageEvents = targetPackageEvents
      , _msCurrentKnowledgeModel = currentKnowledgeModel
      }
