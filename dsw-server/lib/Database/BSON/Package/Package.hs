module Database.BSON.Package.Package where

import qualified Data.Bson as BSON
import Data.Bson.Generic

import Model.Package.Package

instance ToBSON Package where
  toBSON Package {..} =
    [ "id" BSON.=: _packagePId
    , "name" BSON.=: _packageName
    , "organizationId" BSON.=: _packageOrganizationId
    , "kmId" BSON.=: _packageKmId
    , "version" BSON.=: _packageVersion
    , "metamodelVersion" BSON.=: _packageMetamodelVersion
    , "description" BSON.=: _packageDescription
    , "readme" BSON.=: _packageReadme
    , "license" BSON.=: _packageLicense
    , "previousPackageId" BSON.=: _packagePreviousPackageId
    , "forkOfPackageId" BSON.=: _packageForkOfPackageId
    , "mergeCheckpointPackageId" BSON.=: _packageMergeCheckpointPackageId
    , "createdAt" BSON.=: _packageCreatedAt
    ]

instance FromBSON Package where
  fromBSON doc = do
    _packagePId <- BSON.lookup "id" doc
    _packageName <- BSON.lookup "name" doc
    _packageOrganizationId <- BSON.lookup "organizationId" doc
    _packageKmId <- BSON.lookup "kmId" doc
    _packageVersion <- BSON.lookup "version" doc
    _packageMetamodelVersion <- BSON.lookup "metamodelVersion" doc
    _packageDescription <- BSON.lookup "description" doc
    _packageReadme <- BSON.lookup "readme" doc
    _packageLicense <- BSON.lookup "license" doc
    _packagePreviousPackageId <- BSON.lookup "previousPackageId" doc
    _packageForkOfPackageId <- BSON.lookup "forkOfPackageId" doc
    _packageMergeCheckpointPackageId <- BSON.lookup "mergeCheckpointPackageId" doc
    _packageCreatedAt <- BSON.lookup "createdAt" doc
    return Package {..}
