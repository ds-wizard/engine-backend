module Database.BSON.Branch.BranchWithKM where

import qualified Data.Bson as BSON
import Data.Bson.Generic

import Database.BSON.Common
import Database.BSON.KnowledgeModel.KnowledgeModel ()
import Model.Branch.Branch

instance FromBSON BranchWithKM where
  fromBSON doc = do
    bUuid <- deserializeMaybeUUID $ BSON.lookup "uuid" doc
    bName <- BSON.lookup "name" doc
    bKmId <- BSON.lookup "kmId" doc
    bParentPackageId <- BSON.lookup "parentPackageId" doc
    bLastAppliedParentPackageId <- BSON.lookup "lastAppliedParentPackageId" doc
    bLastMergeCheckpointPackageId <- BSON.lookup "lastMergeCheckpointPackageId" doc
    bKmSerialized <- BSON.lookup "knowledgeModel" doc
    let bKm = deserializeKM bKmSerialized
    return
      BranchWithKM
      { _branchWithKMUuid = bUuid
      , _branchWithKMName = bName
      , _branchWithKMKmId = bKmId
      , _branchWithKMParentPackageId = bParentPackageId
      , _branchWithKMLastAppliedParentPackageId = bLastAppliedParentPackageId
      , _branchWithKMLastMergeCheckpointPackageId = bLastMergeCheckpointPackageId
      , _branchWithKMKnowledgeModel = bKm
      }
    where
      deserializeKM (Just kmSerialized) = fromBSON kmSerialized
      deserializeKM Nothing = Nothing
