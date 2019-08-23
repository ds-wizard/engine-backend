module Database.BSON.Branch.Branch where

import qualified Data.Bson as BSON
import Data.Bson.Generic

import Database.BSON.Common ()
import Model.Branch.Branch

instance ToBSON Branch where
  toBSON Branch {..} =
    [ "uuid" BSON.=: _branchUuid
    , "name" BSON.=: _branchName
    , "kmId" BSON.=: _branchKmId
    , "metamodelVersion" BSON.=: _branchMetamodelVersion
    , "previousPackageId" BSON.=: _branchPreviousPackageId
    , "ownerUuid" BSON.=: _branchOwnerUuid
    , "createdAt" BSON.=: _branchCreatedAt
    , "updatedAt" BSON.=: _branchUpdatedAt
    ]

instance FromBSON Branch where
  fromBSON doc = do
    _branchUuid <- BSON.lookup "uuid" doc
    _branchName <- BSON.lookup "name" doc
    _branchKmId <- BSON.lookup "kmId" doc
    _branchMetamodelVersion <- BSON.lookup "metamodelVersion" doc
    _branchPreviousPackageId <- BSON.lookup "previousPackageId" doc
    _branchOwnerUuid <- BSON.lookup "ownerUuid" doc
    _branchCreatedAt <- BSON.lookup "createdAt" doc
    _branchUpdatedAt <- BSON.lookup "updatedAt" doc
    return Branch {..}
