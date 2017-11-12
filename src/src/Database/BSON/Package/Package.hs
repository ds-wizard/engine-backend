module Database.BSON.Package.Package where

import Control.Lens ((^.))
import qualified Data.Bson as BSON
import Data.Bson.Generic
import Data.Maybe
import Data.UUID
import GHC.Generics

import Database.BSON.Common
import Model.Package.Package

instance ToBSON Package where
  toBSON package =
    [ "id" BSON.=: package ^. pkgId
    , "name" BSON.=: (package ^. pkgName)
    , "groupId" BSON.=: (package ^. pkgGroupId)
    , "artifactId" BSON.=: (package ^. pkgArtifactId)
    , "version" BSON.=: (package ^. pkgVersion)
    , "description" BSON.=: (package ^. pkgDescription)
    , "parentPackage" BSON.=: (package ^. pkgParentPackage)
    ]

instance FromBSON Package where
  fromBSON doc = do
    pkgId <- BSON.lookup "id" doc
    name <- BSON.lookup "name" doc
    groupId <- BSON.lookup "groupId" doc
    artifactId <- BSON.lookup "artifactId" doc
    version <- BSON.lookup "version" doc
    description <- BSON.lookup "description" doc
    parentPackage <- BSON.lookup "parentPackage" doc
    return
      Package
      { _pkgId = pkgId
      , _pkgName = name
      , _pkgGroupId = groupId
      , _pkgArtifactId = artifactId
      , _pkgVersion = version
      , _pkgDescription = description
      , _pkgParentPackage = parentPackage
      }
