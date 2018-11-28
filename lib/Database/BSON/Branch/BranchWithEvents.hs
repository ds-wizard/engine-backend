module Database.BSON.Branch.BranchWithEvents where

import qualified Data.Bson as BSON
import Data.Bson.Generic
import Data.Maybe

import Database.BSON.Common
import Database.BSON.Event.Common
import Model.Branch.Branch

instance FromBSON BranchWithEvents where
  fromBSON doc = do
    bUuid <- deserializeMaybeUUID $ BSON.lookup "uuid" doc
    bName <- BSON.lookup "name" doc
    bKmId <- BSON.lookup "kmId" doc
    bParentPackageId <- BSON.lookup "parentPackageId" doc
    bLastAppliedParentPackageId <- BSON.lookup "lastAppliedParentPackageId" doc
    bLastMergeCheckpointPackageId <- BSON.lookup "lastMergeCheckpointPackageId" doc
    bEventsSerialized <- BSON.lookup "events" doc
    let bEvents = fmap (fromJust . chooseEventDeserializator) bEventsSerialized
    let bOwnerUuid = deserializeMaybeUUID $ BSON.lookup "ownerUuid" doc
    bCreatedAt <- BSON.lookup "createdAt" doc
    bUpdatedAt <- BSON.lookup "updatedAt" doc
    return
      BranchWithEvents
      { _branchWithEventsUuid = bUuid
      , _branchWithEventsName = bName
      , _branchWithEventsKmId = bKmId
      , _branchWithEventsParentPackageId = bParentPackageId
      , _branchWithEventsLastAppliedParentPackageId = bLastAppliedParentPackageId
      , _branchWithEventsLastMergeCheckpointPackageId = bLastMergeCheckpointPackageId
      , _branchWithEventsEvents = bEvents
      , _branchWithEventsOwnerUuid = bOwnerUuid
      , _branchWithEventsCreatedAt = bCreatedAt
      , _branchWithEventsUpdatedAt = bUpdatedAt
      }
