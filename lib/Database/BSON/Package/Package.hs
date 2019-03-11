module Database.BSON.Package.Package where

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic

import LensesConfig
import Model.Package.Package

instance ToBSON Package where
  toBSON package =
    [ "id" BSON.=: package ^. pId
    , "name" BSON.=: (package ^. name)
    , "organizationId" BSON.=: (package ^. organizationId)
    , "kmId" BSON.=: (package ^. kmId)
    , "version" BSON.=: (package ^. version)
    , "metamodelVersion" BSON.=: (package ^. metamodelVersion)
    , "description" BSON.=: (package ^. description)
    , "parentPackageId" BSON.=: (package ^. parentPackageId)
    ]

instance FromBSON Package where
  fromBSON doc = do
    pkgPId <- BSON.lookup "id" doc
    pkgName <- BSON.lookup "name" doc
    pkgOrganizationId <- BSON.lookup "organizationId" doc
    pkgKmId <- BSON.lookup "kmId" doc
    pkgVersion <- BSON.lookup "version" doc
    pkgMetamodelVersion <- BSON.lookup "metamodelVersion" doc
    pkgDescription <- BSON.lookup "description" doc
    pkgParentPackageId <- BSON.lookup "parentPackageId" doc
    return
      Package
      { _packagePId = pkgPId
      , _packageName = pkgName
      , _packageOrganizationId = pkgOrganizationId
      , _packageKmId = pkgKmId
      , _packageVersion = pkgVersion
      , _packageMetamodelVersion = pkgMetamodelVersion
      , _packageDescription = pkgDescription
      , _packageParentPackageId = pkgParentPackageId
      }
