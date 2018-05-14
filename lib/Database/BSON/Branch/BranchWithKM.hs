module Database.BSON.Branch.BranchWithKM where

import qualified Data.Bson as BSON
import Data.Bson.Generic

import Database.BSON.Common
import Database.BSON.KnowledgeModel.KnowledgeModel ()
import Model.Branch.Branch

instance FromBSON BranchWithKM where
  fromBSON doc = do
    uuid <- deserializeUUID $ BSON.lookup "uuid" doc
    name <- BSON.lookup "name" doc
    kmId <- BSON.lookup "kmId" doc
    parentPackageId <- BSON.lookup "parentPackageId" doc
    lastAppliedParentPackageId <- BSON.lookup "lastAppliedParentPackageId" doc
    lastMergeCheckpointPackageId <- BSON.lookup "lastMergeCheckpointPackageId" doc
    kmSerialized <- BSON.lookup "knowledgeModel" doc
    let km = deserializeKM kmSerialized
    return
      BranchWithKM
      { _bwkmUuid = uuid
      , _bwkmName = name
      , _bwkmKmId = kmId
      , _bwkmParentPackageId = parentPackageId
      , _bwkmLastAppliedParentPackageId = lastAppliedParentPackageId
      , _bwkmLastMergeCheckpointPackageId = lastMergeCheckpointPackageId
      , _bwkmKM = km
      }
    where
      deserializeKM (Just kmSerialized) = fromBSON kmSerialized
      deserializeKM Nothing = Nothing
