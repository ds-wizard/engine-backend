module Database.BSON.Branch.Branch where

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic

import Database.BSON.Common
import LensesConfig
import Model.Branch.Branch

instance ToBSON Branch where
  toBSON branch =
    [ "uuid" BSON.=: serializeUUID (branch ^. uuid)
    , "name" BSON.=: (branch ^. name)
    , "kmId" BSON.=: (branch ^. kmId)
    , "parentPackageId" BSON.=: (branch ^. parentPackageId)
    , "lastAppliedParentPackageId" BSON.=: (branch ^. lastAppliedParentPackageId)
    , "lastMergeCheckpointPackageId" BSON.=: (branch ^. lastMergeCheckpointPackageId)
    ]

instance FromBSON Branch where
  fromBSON doc = do
    bUuid <- deserializeMaybeUUID $ BSON.lookup "uuid" doc
    bName <- BSON.lookup "name" doc
    bKmId <- BSON.lookup "kmId" doc
    bParentPackageId <- BSON.lookup "parentPackageId" doc
    bLastAppliedParentPackageId <- BSON.lookup "lastAppliedParentPackageId" doc
    bLastMergeCheckpointPackageId <- BSON.lookup "lastMergeCheckpointPackageId" doc
    return
      Branch
      { _branchUuid = bUuid
      , _branchName = bName
      , _branchKmId = bKmId
      , _branchParentPackageId = bParentPackageId
      , _branchLastAppliedParentPackageId = bLastAppliedParentPackageId
      , _branchLastMergeCheckpointPackageId = bLastMergeCheckpointPackageId
      }
