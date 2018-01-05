module Database.BSON.Branch.BranchWithKM where

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic
import Data.Maybe
import Data.UUID
import GHC.Generics

import Database.BSON.Common
import Database.BSON.KnowledgeModel.KnowledgeModel
import Model.Branch.Branch

instance FromBSON BranchWithKM where
  fromBSON doc = do
    uuid <- deserializeUUID $ BSON.lookup "uuid" doc
    name <- BSON.lookup "name" doc
    artifactId <- BSON.lookup "artifactId" doc
    parentPackageId <- BSON.lookup "parentPackageId" doc
    lastAppliedParentPackageId <- BSON.lookup "lastAppliedParentPackageId" doc
    lastMergeCheckpointPackageId <- BSON.lookup "lastMergeCheckpointPackageId" doc
    kmSerialized <- BSON.lookup "knowledgeModel" doc
    let km = deserializeKM kmSerialized
    return
      BranchWithKM
      { _bwkmUuid = uuid
      , _bwkmName = name
      , _bwkmArtifactId = artifactId
      , _bwkmParentPackageId = parentPackageId
      , _bwkmLastAppliedParentPackageId = lastAppliedParentPackageId
      , _bwkmLastMergeCheckpointPackageId = lastMergeCheckpointPackageId
      , _bwkmKM = km
      }
    where
      deserializeKM (Just kmSerialized) = fromBSON kmSerialized
      deserializeKM Nothing = Nothing
