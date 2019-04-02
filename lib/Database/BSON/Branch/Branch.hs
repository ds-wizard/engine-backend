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
    , "metamodelVersion" BSON.=: (branch ^. metamodelVersion)
    , "parentPackageId" BSON.=: (branch ^. parentPackageId)
    , "lastAppliedParentPackageId" BSON.=: (branch ^. lastAppliedParentPackageId)
    , "lastMergeCheckpointPackageId" BSON.=: (branch ^. lastMergeCheckpointPackageId)
    , "ownerUuid" BSON.=: serializeMaybeUUID (branch ^. ownerUuid)
    , "createdAt" BSON.=: (branch ^. createdAt)
    , "updatedAt" BSON.=: (branch ^. updatedAt)
    ]

instance FromBSON Branch where
  fromBSON doc = do
    bUuid <- deserializeMaybeUUID $ BSON.lookup "uuid" doc
    bName <- BSON.lookup "name" doc
    bKmId <- BSON.lookup "kmId" doc
    bMetamodelVersion <- BSON.lookup "metamodelVersion" doc
    bParentPackageId <- BSON.lookup "parentPackageId" doc
    bLastAppliedParentPackageId <- BSON.lookup "lastAppliedParentPackageId" doc
    bLastMergeCheckpointPackageId <- BSON.lookup "lastMergeCheckpointPackageId" doc
    let bOwnerUuid = deserializeMaybeUUID $ BSON.lookup "ownerUuid" doc
    bCreatedAt <- BSON.lookup "createdAt" doc
    bUpdatedAt <- BSON.lookup "updatedAt" doc
    return
      Branch
      { _branchUuid = bUuid
      , _branchName = bName
      , _branchKmId = bKmId
      , _branchMetamodelVersion = bMetamodelVersion
      , _branchParentPackageId = bParentPackageId
      , _branchLastAppliedParentPackageId = bLastAppliedParentPackageId
      , _branchLastMergeCheckpointPackageId = bLastMergeCheckpointPackageId
      , _branchOwnerUuid = bOwnerUuid
      , _branchCreatedAt = bCreatedAt
      , _branchUpdatedAt = bUpdatedAt
      }
