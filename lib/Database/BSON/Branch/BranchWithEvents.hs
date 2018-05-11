module Database.BSON.Branch.BranchWithEvents where

import qualified Data.Bson as BSON
import Data.Bson.Generic
import Data.Maybe

import Database.BSON.Common
import Database.BSON.Event.Common
import Model.Branch.Branch

instance FromBSON BranchWithEvents where
  fromBSON doc = do
    uuid <- deserializeUUID $ BSON.lookup "uuid" doc
    name <- BSON.lookup "name" doc
    artifactId <- BSON.lookup "artifactId" doc
    parentPackageId <- BSON.lookup "parentPackageId" doc
    lastAppliedParentPackageId <- BSON.lookup "lastAppliedParentPackageId" doc
    lastMergeCheckpointPackageId <- BSON.lookup "lastMergeCheckpointPackageId" doc
    eventsSerialized <- BSON.lookup "events" doc
    let events = fmap (fromJust . chooseEventDeserializator) eventsSerialized
    return
      BranchWithEvents
      { _bweUuid = uuid
      , _bweName = name
      , _bweArtifactId = artifactId
      , _bweParentPackageId = parentPackageId
      , _bweLastAppliedParentPackageId = lastAppliedParentPackageId
      , _bweLastMergeCheckpointPackageId = lastMergeCheckpointPackageId
      , _bweEvents = events
      }
